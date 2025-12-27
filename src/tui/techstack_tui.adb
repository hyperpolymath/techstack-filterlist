-------------------------------------------------------------------------------
-- Techstack Enforcer - Terminal User Interface Implementation
-------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Techstack_Enforcer;

package body Techstack_TUI is

   procedure Initialize (State : out TUI_State) is
   begin
      State := TUI_State'
        (Current_View    => Filter_List,
         Selected_Index  => 1,
         Scroll_Offset   => 0,
         Screen_Height   => 24,
         Screen_Width    => 80,
         Running         => True,
         Status_Message  => (others => ' '),
         Status_Len      => 0,
         Status_Is_Error => False);
   end Initialize;

   procedure Set_Status (State : in out TUI_State; Msg : String; Is_Error : Boolean := False) is
      Len : constant Natural := Natural'Min (Msg'Length, State.Status_Message'Length);
   begin
      State.Status_Message := (others => ' ');
      State.Status_Message (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
      State.Status_Len := Len;
      State.Status_Is_Error := Is_Error;
   end Set_Status;

   procedure Move_Cursor (Row, Col : Positive) is
      Row_Str : constant String := Positive'Image (Row);
      Col_Str : constant String := Positive'Image (Col);
   begin
      Put (ESC & "[" & Trim (Row_Str, Ada.Strings.Both) & ";" &
           Trim (Col_Str, Ada.Strings.Both) & "H");
   end Move_Cursor;

   function Get_Level_Color (Level : Block_Level) return String is
   begin
      case Level is
         when Allow => return Green;
         when Warn  => return Yellow;
         when Block => return Magenta;
         when Fatal => return Red;
      end case;
   end Get_Level_Color;

   function Get_Level_Symbol (Level : Block_Level) return Character is
   begin
      case Level is
         when Allow => return '+';
         when Warn  => return '!';
         when Block => return 'X';
         when Fatal => return '#';
      end case;
   end Get_Level_Symbol;

   procedure Render_Header (State : TUI_State) is
      pragma Unreferenced (State);
   begin
      Put (Bold & Cyan);
      Put_Line ("==========================================");
      Put_Line ("     TECHSTACK ENFORCER v1.0              ");
      Put_Line ("     Ada/SPARK Filter Management TUI      ");
      Put_Line ("==========================================");
      Put (Reset);
   end Render_Header;

   procedure Render_Mode_Indicator is
      Mode : constant Enforce_Mode := Techstack_Enforcer.Get_Mode;
   begin
      Put ("  Mode: ");
      case Mode is
         when Learning =>
            Put (Blue & Bold & "[LEARNING]" & Reset);
            Put (" - Observing only, no blocking");
         when Warn_Only =>
            Put (Yellow & Bold & "[WARN]" & Reset);
            Put (" - Warnings only, no blocking");
         when Enforce =>
            Put (Magenta & Bold & "[ENFORCE]" & Reset);
            Put (" - Blocking enabled");
         when Lockdown =>
            Put (Red & Bold & "[LOCKDOWN]" & Reset);
            Put (" - Maximum enforcement");
      end case;
      New_Line;
      New_Line;
   end Render_Mode_Indicator;

   procedure Render_Filter_List (State : TUI_State) is
      Count : constant Natural := Techstack_Enforcer.Get_Filter_Count;
      Display_Start : constant Positive := State.Scroll_Offset + 1;
      Display_End   : Natural;
      Max_Display   : constant := 12;  --  Filters to show at once
   begin
      Put_Line (Bold & "  FILTER LIST (" & Trim (Natural'Image (Count), Ada.Strings.Both) &
                " entries)" & Reset);
      Put_Line (Dim & "  -----------------------------------------------" & Reset);

      if Count = 0 then
         Put_Line ("    No filters defined. Press 'a' to add one.");
         return;
      end if;

      Display_End := Natural'Min (Display_Start + Max_Display - 1, Count);

      for I in Display_Start .. Display_End loop
         declare
            E : constant Filter_Entry := Techstack_Enforcer.Get_Filter (I);
            Pattern : constant String := Trim (E.Pattern (1 .. E.Pattern_Len), Ada.Strings.Both);
            Reason  : constant String := Trim (E.Reason (1 .. Natural'Min (E.Reason_Len, 40)), Ada.Strings.Both);
            Sel_Indicator : constant String := (if I = State.Selected_Index then " > " else "   ");
            Enable_Indicator : constant String := (if E.Enabled then "" else Dim);
            Level_Color : constant String := Get_Level_Color (E.Level);
            Level_Sym : constant Character := Get_Level_Symbol (E.Level);
            Learned_Mark : constant String := (if E.Learned then " [L]" else "");
         begin
            Put (Enable_Indicator);
            if I = State.Selected_Index then
               Put (Bold & BG_Blue & White);
            end if;
            Put (Sel_Indicator);
            Put (Level_Color & "[" & Level_Sym & "]" & Reset);
            if I = State.Selected_Index then
               Put (Bold & BG_Blue & White);
            end if;
            Put (Enable_Indicator);
            Put (" " & Head (Pattern, 25) & " ");
            Put (Dim & "| " & Reason & Learned_Mark);
            Put (Reset);
            New_Line;
         end;
      end loop;

      --  Scroll indicators
      if Display_Start > 1 then
         Put_Line (Dim & "    ... " & Trim (Natural'Image (Display_Start - 1), Ada.Strings.Both) &
                   " more above" & Reset);
      end if;
      if Display_End < Count then
         Put_Line (Dim & "    ... " & Trim (Natural'Image (Count - Display_End), Ada.Strings.Both) &
                   " more below" & Reset);
      end if;
   end Render_Filter_List;

   procedure Render_Add_Filter (State : in out TUI_State) is
      Pattern_Buf : String (1 .. 256) := (others => ' ');
      Reason_Buf  : String (1 .. 256) := (others => ' ');
      Level_Buf   : String (1 .. 10) := (others => ' ');
      Pattern_Len, Reason_Len, Level_Len : Natural;
      Level : Block_Level := Block;
      Success : Boolean;
   begin
      Put (Show_Cursor);
      Put_Line (Bold & Cyan & "  ADD NEW FILTER" & Reset);
      Put_Line ("  -----------------------------------------------");
      New_Line;

      Put ("  Pattern (glob): ");
      Get_Line (Pattern_Buf, Pattern_Len);

      if Pattern_Len = 0 then
         Set_Status (State, "Cancelled - empty pattern", True);
         State.Current_View := Filter_List;
         return;
      end if;

      Put ("  Severity (a=Allow, w=Warn, b=Block, f=Fatal): ");
      Get_Line (Level_Buf, Level_Len);

      if Level_Len > 0 then
         case Level_Buf (1) is
            when 'a' | 'A' => Level := Allow;
            when 'w' | 'W' => Level := Warn;
            when 'b' | 'B' => Level := Block;
            when 'f' | 'F' => Level := Fatal;
            when others => Level := Block;
         end case;
      end if;

      Put ("  Reason: ");
      Get_Line (Reason_Buf, Reason_Len);

      Techstack_Enforcer.Add_Filter
        (Pattern_Buf (1 .. Pattern_Len),
         Level,
         (if Reason_Len > 0 then Reason_Buf (1 .. Reason_Len) else "User-defined filter"),
         Success);

      if Success then
         Set_Status (State, "Filter added: " & Pattern_Buf (1 .. Pattern_Len));
      else
         Set_Status (State, "Failed to add filter", True);
      end if;

      State.Current_View := Filter_List;
      Put (Hide_Cursor);
   end Render_Add_Filter;

   procedure Render_Stats (State : TUI_State) is
      pragma Unreferenced (State);
      Count : constant Natural := Techstack_Enforcer.Get_Filter_Count;
      Total_Hits : Natural := 0;
      Fatal_Count, Block_Count, Warn_Count, Allow_Count : Natural := 0;
      Enabled_Count, Learned_Count : Natural := 0;
   begin
      Put_Line (Bold & Cyan & "  FILTER STATISTICS" & Reset);
      Put_Line ("  -----------------------------------------------");
      New_Line;

      for I in 1 .. Count loop
         declare
            E : constant Filter_Entry := Techstack_Enforcer.Get_Filter (I);
         begin
            Total_Hits := Total_Hits + E.Hit_Count;
            if E.Enabled then
               Enabled_Count := Enabled_Count + 1;
            end if;
            if E.Learned then
               Learned_Count := Learned_Count + 1;
            end if;
            case E.Level is
               when Fatal => Fatal_Count := Fatal_Count + 1;
               when Block => Block_Count := Block_Count + 1;
               when Warn => Warn_Count := Warn_Count + 1;
               when Allow => Allow_Count := Allow_Count + 1;
            end case;
         end;
      end loop;

      Put_Line ("  Total Filters:     " & Natural'Image (Count));
      Put_Line ("  Enabled Filters:   " & Natural'Image (Enabled_Count));
      Put_Line ("  Learned Filters:   " & Natural'Image (Learned_Count));
      New_Line;
      Put_Line ("  By Severity:");
      Put (Red);    Put_Line ("    Fatal:  " & Natural'Image (Fatal_Count));
      Put (Magenta); Put_Line ("    Block:  " & Natural'Image (Block_Count));
      Put (Yellow);  Put_Line ("    Warn:   " & Natural'Image (Warn_Count));
      Put (Green);   Put_Line ("    Allow:  " & Natural'Image (Allow_Count));
      Put (Reset);
      New_Line;
      Put_Line ("  Total Pattern Hits:" & Natural'Image (Total_Hits));
      New_Line;
      Put_Line (Dim & "  Press any key to return..." & Reset);
   end Render_Stats;

   procedure Render_Help (State : TUI_State) is
      pragma Unreferenced (State);
   begin
      Put_Line (Bold & Cyan & "  KEYBOARD SHORTCUTS" & Reset);
      Put_Line ("  -----------------------------------------------");
      New_Line;
      Put_Line ("  Navigation:");
      Put_Line ("    " & Bold & "j/k" & Reset & " or " & Bold & "Down/Up" & Reset & "  - Move selection");
      Put_Line ("    " & Bold & "g" & Reset & "           - Go to first filter");
      Put_Line ("    " & Bold & "G" & Reset & "           - Go to last filter");
      New_Line;
      Put_Line ("  Filter Management:");
      Put_Line ("    " & Bold & "a" & Reset & "           - Add new filter");
      Put_Line ("    " & Bold & "d" & Reset & "           - Delete selected filter");
      Put_Line ("    " & Bold & "e" & Reset & "           - Edit selected filter");
      Put_Line ("    " & Bold & "Space" & Reset & "       - Toggle enabled/disabled");
      New_Line;
      Put_Line ("  Enforcement Mode:");
      Put_Line ("    " & Bold & "1" & Reset & "           - Learning mode (observe only)");
      Put_Line ("    " & Bold & "2" & Reset & "           - Warn only mode");
      Put_Line ("    " & Bold & "3" & Reset & "           - Enforce mode (block violations)");
      Put_Line ("    " & Bold & "4" & Reset & "           - Lockdown mode (maximum security)");
      New_Line;
      Put_Line ("  Other:");
      Put_Line ("    " & Bold & "s" & Reset & "           - Scan repository");
      Put_Line ("    " & Bold & "S" & Reset & "           - View statistics");
      Put_Line ("    " & Bold & "r" & Reset & "           - Reset hit counters");
      Put_Line ("    " & Bold & "w" & Reset & "           - Save configuration");
      Put_Line ("    " & Bold & "?" & Reset & "           - Show this help");
      Put_Line ("    " & Bold & "q" & Reset & "           - Quit");
      New_Line;
      Put_Line (Dim & "  Press any key to return..." & Reset);
   end Render_Help;

   procedure Render_Scan_Repo (State : in out TUI_State) is
      Path_Buf : String (1 .. 1024) := (others => ' ');
      Path_Len : Natural;
      Total, Violations, Fatals : Natural;
      Success : Boolean;
   begin
      Put (Show_Cursor);
      Put_Line (Bold & Cyan & "  SCAN REPOSITORY" & Reset);
      Put_Line ("  -----------------------------------------------");
      New_Line;

      Put ("  Enter repository path: ");
      Get_Line (Path_Buf, Path_Len);

      if Path_Len = 0 then
         Set_Status (State, "Cancelled - empty path", True);
         State.Current_View := Filter_List;
         return;
      end if;

      Put_Line ("  Scanning...");

      Techstack_Enforcer.Check_Repository
        (Path_Buf (1 .. Path_Len), Total, Violations, Fatals, Success);

      New_Line;
      if Success then
         Put_Line ("  " & Bold & "Scan Results:" & Reset);
         Put_Line ("    Total files scanned:" & Natural'Image (Total));
         if Violations > 0 then
            Put (Red);
         else
            Put (Green);
         end if;
         Put_Line ("    Violations found:   " & Natural'Image (Violations));
         Put (Reset);
         if Fatals > 0 then
            Put (Bold & Red);
            Put_Line ("    FATAL BLOCKS:       " & Natural'Image (Fatals));
            Put (Reset);
         end if;
         New_Line;
         if Violations = 0 then
            Put_Line (Green & "  Repository is CLEAN!" & Reset);
         else
            Put_Line (Red & "  Repository has VIOLATIONS!" & Reset);
         end if;
      else
         Put_Line (Red & "  Scan FAILED - check path exists" & Reset);
      end if;

      New_Line;
      Put_Line (Dim & "  Press any key to return..." & Reset);

      declare
         Dummy : String (1 .. 10);
         Dummy_Len : Natural;
      begin
         Get_Line (Dummy, Dummy_Len);
      end;

      State.Current_View := Filter_List;
      Put (Hide_Cursor);
   end Render_Scan_Repo;

   procedure Render_Edit_Filter (State : in out TUI_State) is
      Pattern_Buf : String (1 .. 256) := (others => ' ');
      Reason_Buf  : String (1 .. 256) := (others => ' ');
      Level_Buf   : String (1 .. 10) := (others => ' ');
      Pattern_Len, Reason_Len, Level_Len : Natural;
      New_Level : Block_Level := Block;
      Success : Boolean;
      Current_Filter : Filter_Entry;
      Current_Pattern, Current_Reason : String (1 .. 256) := (others => ' ');
      Cur_Pat_Len, Cur_Reas_Len : Natural;
   begin
      --  Get current filter data
      Current_Filter := Techstack_Enforcer.Get_Filter (State.Selected_Index);
      Cur_Pat_Len := Current_Filter.Pattern_Len;
      Cur_Reas_Len := Current_Filter.Reason_Len;
      Current_Pattern (1 .. Cur_Pat_Len) := Current_Filter.Pattern (1 .. Cur_Pat_Len);
      Current_Reason (1 .. Cur_Reas_Len) := Current_Filter.Reason (1 .. Cur_Reas_Len);

      Put (Show_Cursor);
      Put_Line (Bold & Cyan & "  EDIT FILTER" & Reset);
      Put_Line ("  -----------------------------------------------");
      New_Line;

      --  Show current values
      Put_Line ("  Current values:");
      Put ("    Pattern: ");
      Put (Green);
      Put_Line (Trim (Current_Pattern (1 .. Cur_Pat_Len), Ada.Strings.Both));
      Put (Reset);
      Put ("    Level:   ");
      Put (Get_Level_Color (Current_Filter.Level));
      case Current_Filter.Level is
         when Allow => Put_Line ("Allow");
         when Warn  => Put_Line ("Warn");
         when Block => Put_Line ("Block");
         when Fatal => Put_Line ("Fatal");
      end case;
      Put (Reset);
      Put ("    Reason:  ");
      Put_Line (Trim (Current_Reason (1 .. Cur_Reas_Len), Ada.Strings.Both));
      Put ("    Enabled: ");
      if Current_Filter.Enabled then
         Put_Line (Green & "Yes" & Reset);
      else
         Put_Line (Red & "No" & Reset);
      end if;
      New_Line;

      Put_Line ("  Enter new values (press Enter to keep current):");
      New_Line;

      Put ("  Pattern [" & Trim (Current_Pattern (1 .. Cur_Pat_Len), Ada.Strings.Both) & "]: ");
      Get_Line (Pattern_Buf, Pattern_Len);

      --  Use current value if empty
      if Pattern_Len = 0 then
         Pattern_Buf (1 .. Cur_Pat_Len) := Current_Pattern (1 .. Cur_Pat_Len);
         Pattern_Len := Cur_Pat_Len;
      end if;

      Put ("  Level (a/w/b/f) [");
      case Current_Filter.Level is
         when Allow => Put ("a");
         when Warn  => Put ("w");
         when Block => Put ("b");
         when Fatal => Put ("f");
      end case;
      Put ("]: ");
      Get_Line (Level_Buf, Level_Len);

      if Level_Len > 0 then
         case Level_Buf (1) is
            when 'a' | 'A' => New_Level := Allow;
            when 'w' | 'W' => New_Level := Warn;
            when 'b' | 'B' => New_Level := Block;
            when 'f' | 'F' => New_Level := Fatal;
            when others => New_Level := Current_Filter.Level;
         end case;
      else
         New_Level := Current_Filter.Level;
      end if;

      Put ("  Reason [" & Trim (Current_Reason (1 .. Cur_Reas_Len), Ada.Strings.Both) & "]: ");
      Get_Line (Reason_Buf, Reason_Len);

      if Reason_Len = 0 then
         Reason_Buf (1 .. Cur_Reas_Len) := Current_Reason (1 .. Cur_Reas_Len);
         Reason_Len := Cur_Reas_Len;
      end if;

      --  Delete old filter and add updated one
      Techstack_Enforcer.Remove_Filter (State.Selected_Index, Success);

      if Success then
         Techstack_Enforcer.Add_Filter
           (Pattern_Buf (1 .. Pattern_Len),
            New_Level,
            Reason_Buf (1 .. Reason_Len),
            Success);

         if Success then
            Set_Status (State, "Filter updated: " & Trim (Pattern_Buf (1 .. Pattern_Len), Ada.Strings.Both));
         else
            Set_Status (State, "Failed to add updated filter", True);
         end if;
      else
         Set_Status (State, "Failed to remove old filter", True);
      end if;

      State.Current_View := Filter_List;
      Put (Hide_Cursor);
   end Render_Edit_Filter;

   procedure Render_Status_Bar (State : TUI_State) is
   begin
      Move_Cursor (State.Screen_Height, 1);
      if State.Status_Is_Error then
         Put (Bold & Red);
      else
         Put (Dim);
      end if;
      Put ("  " & State.Status_Message (1 .. State.Status_Len));
      Put (Reset);
   end Render_Status_Bar;

   procedure Handle_Filter_List_Key (State : in out TUI_State; Key : Character) is
      Count : constant Natural := Techstack_Enforcer.Get_Filter_Count;
      Success : Boolean;
   begin
      case Key is
         --  Navigation
         when 'j' | Character'Val (66) =>  --  Down arrow is ESC[B, we get 'B' (66)
            if State.Selected_Index < Count then
               State.Selected_Index := State.Selected_Index + 1;
            end if;
         when 'k' | Character'Val (65) =>  --  Up arrow
            if State.Selected_Index > 1 then
               State.Selected_Index := State.Selected_Index - 1;
            end if;
         when 'g' =>
            State.Selected_Index := 1;
            State.Scroll_Offset := 0;
         when 'G' =>
            if Count > 0 then
               State.Selected_Index := Count;
            end if;

         --  Filter management
         when 'a' =>
            State.Current_View := Add_Filter;
         when 'd' =>
            if Count > 0 then
               Techstack_Enforcer.Remove_Filter (State.Selected_Index, Success);
               if Success then
                  Set_Status (State, "Filter deleted");
                  if State.Selected_Index > Techstack_Enforcer.Get_Filter_Count and
                     State.Selected_Index > 1
                  then
                     State.Selected_Index := State.Selected_Index - 1;
                  end if;
               else
                  Set_Status (State, "Failed to delete filter", True);
               end if;
            end if;
         when 'e' =>
            if Count > 0 then
               State.Current_View := Edit_Filter;
            else
               Set_Status (State, "No filter to edit", True);
            end if;
         when ' ' =>  --  Space to toggle
            if Count > 0 then
               Techstack_Enforcer.Toggle_Filter (State.Selected_Index, Success);
               if Success then
                  Set_Status (State, "Filter toggled");
               end if;
            end if;

         --  Mode switching
         when '1' =>
            Techstack_Enforcer.Set_Mode (Learning);
            Set_Status (State, "Mode: LEARNING - observing only");
         when '2' =>
            Techstack_Enforcer.Set_Mode (Warn_Only);
            Set_Status (State, "Mode: WARN - warnings only, no blocking");
         when '3' =>
            Techstack_Enforcer.Set_Mode (Enforce);
            Set_Status (State, "Mode: ENFORCE - blocking enabled");
         when '4' =>
            Techstack_Enforcer.Set_Mode (Lockdown);
            Set_Status (State, "Mode: LOCKDOWN - maximum enforcement!");

         --  Other actions
         when 's' =>
            State.Current_View := Scan_Repo;
         when 'S' =>
            State.Current_View := Stats;
         when 'r' =>
            Techstack_Enforcer.Reset_Stats;
            Set_Status (State, "Statistics reset");
         when 'w' =>
            declare
               Save_Success : Boolean;
            begin
               Techstack_Enforcer.Save_Config ("techstack.toml", Save_Success);
               if Save_Success then
                  Set_Status (State, "Configuration saved to techstack.toml");
               else
                  Set_Status (State, "Failed to save configuration", True);
               end if;
            end;
         when '?' =>
            State.Current_View := Help;
         when 'q' =>
            State.Running := False;

         when others =>
            null;
      end case;
   end Handle_Filter_List_Key;

   procedure Handle_Key (State : in out TUI_State; Key : Character) is
   begin
      case State.Current_View is
         when Filter_List =>
            Handle_Filter_List_Key (State, Key);
         when Add_Filter =>
            --  Handled in Render_Add_Filter
            null;
         when Edit_Filter =>
            --  Similar to Add_Filter
            State.Current_View := Filter_List;
         when Stats | Help =>
            State.Current_View := Filter_List;
         when Scan_Repo =>
            --  Handled in Render_Scan_Repo
            null;
      end case;
   end Handle_Key;

   procedure Run is
      State : TUI_State;
      Input : Character;
   begin
      --  Initialize
      Initialize (State);
      Techstack_Enforcer.Initialize;

      --  Setup terminal
      Put (Hide_Cursor);

      --  Main loop
      while State.Running loop
         --  Clear and render
         Put (Clear_Screen);
         Render_Header (State);
         Render_Mode_Indicator;

         case State.Current_View is
            when Filter_List =>
               Render_Filter_List (State);
               New_Line;
               Put_Line (Dim & "  [a]dd [e]dit [d]elete [Space]toggle [1-4]mode [s]can [?]help [q]uit" & Reset);
            when Add_Filter =>
               Render_Add_Filter (State);
            when Edit_Filter =>
               Render_Edit_Filter (State);
            when Stats =>
               Render_Stats (State);
            when Help =>
               Render_Help (State);
            when Scan_Repo =>
               Render_Scan_Repo (State);
         end case;

         Render_Status_Bar (State);

         --  Wait for input
         if State.Current_View = Filter_List or
            State.Current_View = Stats or
            State.Current_View = Help
         then
            Get_Immediate (Input);
            Handle_Key (State, Input);
         end if;
      end loop;

      --  Cleanup
      Put (Show_Cursor);
      Put (Clear_Screen);
      Put_Line ("Techstack Enforcer terminated.");

   exception
      when others =>
         Put (Show_Cursor);
         Put (Reset);
   end Run;

end Techstack_TUI;

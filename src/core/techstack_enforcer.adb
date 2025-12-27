-------------------------------------------------------------------------------
-- Techstack Enforcer - Core Enforcement Engine Implementation
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Techstack_Patterns;

package body Techstack_Enforcer with SPARK_Mode => Off is

   use Ada.Text_IO;
   use Ada.Directories;
   use Ada.Strings.Fixed;

   --  Helper to create bounded pattern string
   function To_Pattern (S : String) return Pattern_String is
      Result : Pattern_String := (others => ' ');
   begin
      if S'Length <= Max_Pattern_Length then
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end To_Pattern;

   --  Helper to create bounded reason string
   function To_Reason (S : String) return Reason_String is
      Result : Reason_String := (others => ' ');
   begin
      if S'Length <= Max_Reason_Length then
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end To_Reason;

   --  Helper to create bounded path string
   function To_Path (S : String) return Path_String is
      Result : Path_String := (others => ' ');
   begin
      if S'Length <= Max_Path_Length then
         Result (1 .. S'Length) := S;
      end if;
      return Result;
   end To_Path;

   procedure Initialize is
   begin
      DB.Count := 0;
      DB.Mode := Enforce;
      DB.Entries := (others => Null_Filter_Entry);

      --  Add default safety-critical filters
      declare
         Success : Boolean;
      begin
         --  Python ecosystem (memory-unsafe, runtime errors)
         Add_Filter ("*.py", Fatal, "Python: No static typing, memory-unsafe runtime", Success);
         Add_Filter ("*.pyc", Fatal, "Python bytecode detected", Success);
         Add_Filter ("*.pyo", Fatal, "Python optimized bytecode", Success);
         Add_Filter ("requirements.txt", Fatal, "Python dependency file - use memory-safe language", Success);
         Add_Filter ("Pipfile", Fatal, "Python Pipenv detected", Success);
         Add_Filter ("pyproject.toml", Fatal, "Python project detected", Success);
         Add_Filter ("setup.py", Fatal, "Python setup script", Success);
         Add_Filter ("__pycache__", Fatal, "Python cache directory", Success);

         --  Docker (prefer Podman)
         Add_Filter ("Dockerfile", Block, "Use Containerfile for Podman instead", Success);
         Add_Filter ("docker-compose.yml", Block, "Use podman-compose instead", Success);
         Add_Filter ("docker-compose.yaml", Block, "Use podman-compose instead", Success);
         Add_Filter (".dockerignore", Warn, "Consider .containerignore for Podman", Success);

         --  JavaScript/Node (type-unsafe, runtime errors)
         Add_Filter ("*.js", Block, "JavaScript: Use TypeScript or ReScript", Success);
         Add_Filter ("*.jsx", Block, "JSX: Use TypeScript TSX or ReScript", Success);
         Add_Filter ("package-lock.json", Warn, "Node.js project detected", Success);

         --  C/C++ (memory-unsafe)
         Add_Filter ("*.c", Block, "C: Memory-unsafe, use Rust or Ada/SPARK", Success);
         Add_Filter ("*.cpp", Block, "C++: Memory-unsafe, use Rust or Ada/SPARK", Success);
         Add_Filter ("*.cc", Block, "C++: Memory-unsafe, use Rust or Ada/SPARK", Success);
         Add_Filter ("*.h", Warn, "C header: Consider memory-safe alternatives", Success);
         Add_Filter ("*.hpp", Warn, "C++ header: Consider memory-safe alternatives", Success);

         --  GitHub-specific (prefer GitLab)
         Add_Filter (".github", Warn, "GitHub config: Consider GitLab CI", Success);

         --  Allow-list for preferred stacks
         --  (These would be processed differently in actual logic)
      end;
   end Initialize;

   --  Helper to extract quoted string value from TOML line
   --  Format: key = "value" or key = 'value'
   function Extract_Quoted_Value (Line : String) return String is
      Eq_Pos : Natural := 0;
      Start_Quote, End_Quote : Natural := 0;
      Quote_Char : Character := '"';
   begin
      --  Find equals sign
      for I in Line'Range loop
         if Line (I) = '=' then
            Eq_Pos := I;
            exit;
         end if;
      end loop;

      if Eq_Pos = 0 or Eq_Pos >= Line'Last then
         return "";
      end if;

      --  Find opening quote
      for I in Eq_Pos + 1 .. Line'Last loop
         if Line (I) = '"' or Line (I) = ''' then
            Start_Quote := I;
            Quote_Char := Line (I);
            exit;
         end if;
      end loop;

      if Start_Quote = 0 or Start_Quote >= Line'Last then
         return "";
      end if;

      --  Find closing quote
      for I in Start_Quote + 1 .. Line'Last loop
         if Line (I) = Quote_Char then
            End_Quote := I;
            exit;
         end if;
      end loop;

      if End_Quote = 0 or End_Quote <= Start_Quote + 1 then
         return "";
      end if;

      return Line (Start_Quote + 1 .. End_Quote - 1);
   end Extract_Quoted_Value;

   --  Helper to check if line starts with key
   function Starts_With_Key (Line : String; Key : String) return Boolean is
      Trimmed : constant String := Trim (Line, Ada.Strings.Both);
   begin
      if Trimmed'Length < Key'Length then
         return False;
      end if;
      return Trimmed (Trimmed'First .. Trimmed'First + Key'Length - 1) = Key;
   end Starts_With_Key;

   procedure Load_Config (Config_Path : String; Success : out Boolean) is
      File : File_Type;
      Line : String (1 .. 1024);
      Last : Natural;
      In_Block : Boolean := False;
      In_Mode : Boolean := False;

      --  Current block being parsed
      Cur_Pattern : Pattern_String := (others => ' ');
      Cur_Pattern_Len : Natural := 0;
      Cur_Reason : Reason_String := (others => ' ');
      Cur_Reason_Len : Natural := 0;
      Cur_Level : Block_Level := Block;
      Cur_Enabled : Boolean := True;
      Has_Pattern : Boolean := False;

      Add_Success : Boolean;
   begin
      Success := False;
      if not Exists (Config_Path) then
         return;
      end if;

      Open (File, In_File, Config_Path);

      --  Clear user filters (keep only if fresh load desired)
      --  DB.Count := 0;  -- Uncomment to clear defaults

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);

         if Last = 0 then
            --  Empty line, skip
            null;
         elsif Line (1) = '#' then
            --  Comment, skip
            null;
         elsif Last >= 9 and then Line (1 .. 9) = "[[block]]" then
            --  Save previous block if we have one
            if In_Block and Has_Pattern then
               Add_Filter (Trim (Cur_Pattern (1 .. Cur_Pattern_Len), Ada.Strings.Both),
                           Cur_Level,
                           (if Cur_Reason_Len > 0
                            then Trim (Cur_Reason (1 .. Cur_Reason_Len), Ada.Strings.Both)
                            else ""),
                           Add_Success);
               --  Set enabled state
               if Add_Success and not Cur_Enabled then
                  Toggle_Filter (DB.Count, Add_Success);
               end if;
            end if;

            --  Start new block
            In_Block := True;
            In_Mode := False;
            Has_Pattern := False;
            Cur_Pattern := (others => ' ');
            Cur_Pattern_Len := 0;
            Cur_Reason := (others => ' ');
            Cur_Reason_Len := 0;
            Cur_Level := Block;
            Cur_Enabled := True;

         elsif Last >= 6 and then Line (1 .. 6) = "[mode]" then
            --  Save previous block if we have one
            if In_Block and Has_Pattern then
               Add_Filter (Trim (Cur_Pattern (1 .. Cur_Pattern_Len), Ada.Strings.Both),
                           Cur_Level,
                           (if Cur_Reason_Len > 0
                            then Trim (Cur_Reason (1 .. Cur_Reason_Len), Ada.Strings.Both)
                            else ""),
                           Add_Success);
            end if;
            In_Block := False;
            In_Mode := True;

         elsif In_Mode then
            --  Parse mode settings
            if Starts_With_Key (Line (1 .. Last), "enforce") then
               declare
                  Val : constant String := Extract_Quoted_Value (Line (1 .. Last));
               begin
                  if Val = "learning" then
                     DB.Mode := Learning;
                  elsif Val = "warn" then
                     DB.Mode := Warn_Only;
                  elsif Val = "enforce" then
                     DB.Mode := Enforce;
                  elsif Val = "lockdown" then
                     DB.Mode := Lockdown;
                  end if;
               end;
            end if;

         elsif In_Block then
            --  Parse block fields
            if Starts_With_Key (Line (1 .. Last), "pattern") then
               declare
                  Val : constant String := Extract_Quoted_Value (Line (1 .. Last));
               begin
                  if Val'Length > 0 and Val'Length <= Max_Pattern_Length then
                     Cur_Pattern_Len := Val'Length;
                     Cur_Pattern (1 .. Cur_Pattern_Len) := Val;
                     Has_Pattern := True;
                  end if;
               end;

            elsif Starts_With_Key (Line (1 .. Last), "level") then
               declare
                  Val : constant String := Extract_Quoted_Value (Line (1 .. Last));
               begin
                  if Val = "allow" then
                     Cur_Level := Allow;
                  elsif Val = "warn" then
                     Cur_Level := Warn;
                  elsif Val = "block" then
                     Cur_Level := Block;
                  elsif Val = "fatal" then
                     Cur_Level := Fatal;
                  end if;
               end;

            elsif Starts_With_Key (Line (1 .. Last), "reason") then
               declare
                  Val : constant String := Extract_Quoted_Value (Line (1 .. Last));
               begin
                  if Val'Length > 0 and Val'Length <= Max_Reason_Length then
                     Cur_Reason_Len := Val'Length;
                     Cur_Reason (1 .. Cur_Reason_Len) := Val;
                  end if;
               end;

            elsif Starts_With_Key (Line (1 .. Last), "enabled") then
               --  Check for false value
               if Index (Line (1 .. Last), "false") > 0 then
                  Cur_Enabled := False;
               else
                  Cur_Enabled := True;
               end if;
            end if;
         end if;
      end loop;

      --  Don't forget the last block
      if In_Block and Has_Pattern then
         Add_Filter (Trim (Cur_Pattern (1 .. Cur_Pattern_Len), Ada.Strings.Both),
                     Cur_Level,
                     (if Cur_Reason_Len > 0
                      then Trim (Cur_Reason (1 .. Cur_Reason_Len), Ada.Strings.Both)
                      else ""),
                     Add_Success);
         if Add_Success and not Cur_Enabled then
            Toggle_Filter (DB.Count, Add_Success);
         end if;
      end if;

      Close (File);
      Success := True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Success := False;
   end Load_Config;

   procedure Save_Config (Config_Path : String; Success : out Boolean) is
      File : File_Type;
   begin
      Success := False;

      Create (File, Out_File, Config_Path);

      --  Write header
      Put_Line (File, "# Techstack Enforcer Configuration");
      Put_Line (File, "# Auto-generated - edit with techstack-tui");
      New_Line (File);

      --  Write mode
      Put_Line (File, "[mode]");
      case DB.Mode is
         when Learning => Put_Line (File, "enforce = ""learning""");
         when Warn_Only => Put_Line (File, "enforce = ""warn""");
         when Enforce => Put_Line (File, "enforce = ""enforce""");
         when Lockdown => Put_Line (File, "enforce = ""lockdown""");
      end case;
      New_Line (File);

      --  Write filters
      for I in 1 .. DB.Count loop
         declare
            E : constant Filter_Entry := DB.Entries (I);
         begin
            Put_Line (File, "[[block]]");
            Put_Line (File, "pattern = """ & Trim (E.Pattern (1 .. E.Pattern_Len), Ada.Strings.Both) & """");
            case E.Level is
               when Allow => Put_Line (File, "level = ""allow""");
               when Warn => Put_Line (File, "level = ""warn""");
               when Block => Put_Line (File, "level = ""block""");
               when Fatal => Put_Line (File, "level = ""fatal""");
            end case;
            Put_Line (File, "reason = """ & Trim (E.Reason (1 .. E.Reason_Len), Ada.Strings.Both) & """");
            Put_Line (File, "enabled = " & (if E.Enabled then "true" else "false"));
            Put_Line (File, "learned = " & (if E.Learned then "true" else "false"));
            New_Line (File);
         end;
      end loop;

      Close (File);
      Success := True;

   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         Success := False;
   end Save_Config;

   procedure Check_File
     (File_Path : Path_String;
      Path_Len  : Natural;
      Result    : out Check_Result)
   is
      use Techstack_Patterns;
   begin
      Result := Null_Result;

      --  Skip if in learning mode (only log, don't block)
      if DB.Mode = Learning then
         Result.Blocked := False;
         return;
      end if;

      --  Check against all filters
      for I in 1 .. DB.Count loop
         declare
            E : Filter_Entry renames DB.Entries (I);
         begin
            if E.Enabled then
               if Match_Pattern (E.Pattern, E.Pattern_Len, File_Path, Path_Len) then
                  --  Match found
                  DB.Entries (I).Hit_Count := DB.Entries (I).Hit_Count + 1;

                  --  Determine if this triggers blocking
                  case DB.Mode is
                     when Learning =>
                        --  Just record, don't block
                        null;

                     when Warn_Only =>
                        --  Never block, but record
                        if E.Level > Result.Level then
                           Result.Level := E.Level;
                           Result.Pattern := E.Pattern;
                           Result.Pattern_Len := E.Pattern_Len;
                           Result.Reason := E.Reason;
                           Result.Reason_Len := E.Reason_Len;
                        end if;

                     when Enforce =>
                        --  Block on Block or Fatal
                        if E.Level >= Block then
                           Result.Blocked := True;
                           if E.Level > Result.Level then
                              Result.Level := E.Level;
                              Result.Pattern := E.Pattern;
                              Result.Pattern_Len := E.Pattern_Len;
                              Result.Reason := E.Reason;
                              Result.Reason_Len := E.Reason_Len;
                           end if;
                        end if;

                     when Lockdown =>
                        --  Block on Warn or higher
                        if E.Level >= Warn then
                           Result.Blocked := True;
                           if E.Level > Result.Level then
                              Result.Level := E.Level;
                              Result.Pattern := E.Pattern;
                              Result.Pattern_Len := E.Pattern_Len;
                              Result.Reason := E.Reason;
                              Result.Reason_Len := E.Reason_Len;
                           end if;
                        end if;
                  end case;

                  --  Fatal always blocks regardless of mode (except Learning)
                  if E.Level = Fatal and DB.Mode /= Learning then
                     Result.Blocked := True;
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Check_File;

   procedure Check_Repository
     (Repo_Path     : String;
      Total_Files   : out Natural;
      Violations    : out Natural;
      Fatal_Blocks  : out Natural;
      Success       : out Boolean)
   is
      procedure Scan_Directory (Dir : String) is
         Search : Search_Type;
         Dir_Entry : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir, "*");
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Dir_Entry);
            declare
               Name : constant String := Simple_Name (Dir_Entry);
               Full : constant String := Full_Name (Dir_Entry);
            begin
               --  Skip . and ..
               if Name /= "." and Name /= ".." then
                  if Kind (Dir_Entry) = Directory then
                     --  Skip .git directory
                     if Name /= ".git" then
                        Scan_Directory (Full);
                     end if;
                  elsif Kind (Dir_Entry) = Ordinary_File then
                     Total_Files := Total_Files + 1;

                     declare
                        Path : constant Path_String := To_Path (Full);
                        Path_Len : constant Natural :=
                          (if Full'Length <= Max_Path_Length then Full'Length else 0);
                        Result : Check_Result;
                     begin
                        if Path_Len > 0 then
                           Check_File (Path, Path_Len, Result);
                           if Result.Blocked then
                              Violations := Violations + 1;
                              if Result.Level = Fatal then
                                 Fatal_Blocks := Fatal_Blocks + 1;
                              end if;
                           end if;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;
         End_Search (Search);
      exception
         when others => null;  --  Skip inaccessible directories
      end Scan_Directory;

   begin
      Total_Files := 0;
      Violations := 0;
      Fatal_Blocks := 0;
      Success := False;

      if not Exists (Repo_Path) or else Kind (Repo_Path) /= Directory then
         return;
      end if;

      Scan_Directory (Repo_Path);
      Success := True;

   exception
      when others =>
         Success := False;
   end Check_Repository;

   procedure Add_Filter
     (Pattern     : String;
      Level       : Block_Level;
      Reason      : String;
      Success     : out Boolean)
   is
   begin
      Success := False;

      if DB.Count >= Max_Filter_Entries then
         return;
      end if;

      if Pattern'Length = 0 or Pattern'Length > Max_Pattern_Length then
         return;
      end if;

      DB.Count := DB.Count + 1;
      DB.Entries (DB.Count) := Filter_Entry'
        (Pattern      => To_Pattern (Pattern),
         Pattern_Len  => Pattern'Length,
         Level        => Level,
         Reason       => To_Reason (Reason),
         Reason_Len   => (if Reason'Length <= Max_Reason_Length then Reason'Length else 0),
         Enabled      => True,
         Learned      => False,
         Hit_Count    => 0);

      Success := True;
   end Add_Filter;

   procedure Remove_Filter (Index : Positive; Success : out Boolean) is
   begin
      Success := False;

      if Index > DB.Count then
         return;
      end if;

      --  Shift entries down
      for I in Index .. DB.Count - 1 loop
         DB.Entries (I) := DB.Entries (I + 1);
      end loop;

      DB.Entries (DB.Count) := Null_Filter_Entry;
      DB.Count := DB.Count - 1;
      Success := True;
   end Remove_Filter;

   procedure Toggle_Filter (Index : Positive; Success : out Boolean) is
   begin
      Success := False;

      if Index > DB.Count then
         return;
      end if;

      DB.Entries (Index).Enabled := not DB.Entries (Index).Enabled;
      Success := True;
   end Toggle_Filter;

   procedure Set_Mode (Mode : Enforce_Mode) is
   begin
      DB.Mode := Mode;
   end Set_Mode;

   function Get_Mode return Enforce_Mode is
   begin
      return DB.Mode;
   end Get_Mode;

   function Get_Filter_Count return Natural is
   begin
      return DB.Count;
   end Get_Filter_Count;

   function Get_Filter (Index : Positive) return Filter_Entry is
   begin
      if Index <= DB.Count then
         return DB.Entries (Index);
      else
         return Null_Filter_Entry;
      end if;
   end Get_Filter;

   procedure Reset_Stats is
   begin
      for I in 1 .. DB.Count loop
         DB.Entries (I).Hit_Count := 0;
      end loop;
   end Reset_Stats;

end Techstack_Enforcer;

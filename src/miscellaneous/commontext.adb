--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Strings;

package body CommonText is

   package AS  renames Ada.Strings;
   package UES renames Ada.Strings.UTF_Encoding.Strings;

   -----------
   --  USS  --
   -----------
   function USS (US : Text) return String is
   begin
      return SU.To_String (US);
   end USS;


   -----------
   --  SUS  --
   -----------
   function SUS (S : String) return Text is
   begin
      return SU.To_Unbounded_String (S);
   end SUS;


   -------------
   --  UTF8S  --
   -------------
   function UTF8S (S8 : UTF8) return String is
   begin
      return UES.Decode (S8);
   end UTF8S;


   -------------
   --  SUTF8  --
   -------------
   function SUTF8 (S : String) return UTF8 is
   begin
      return UES.Encode (S);
   end SUTF8;


   -----------------
   --  IsBlank #1 --
   -----------------
   function IsBlank (US : Text)   return Boolean is
   begin
      return SU.Length (US) = 0;
   end IsBlank;


   -----------------
   --  IsBlank #2 --
   -----------------
   function IsBlank (S  : String) return Boolean is
   begin
      return S'Length = 0;
   end IsBlank;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A, B : Text) return Boolean
   is
      use type Text;
   begin
      return A = B;
   end equivalent;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A : Text; B : String) return Boolean
   is
      AS : constant String := USS (A);
   begin
      return AS = B;
   end equivalent;


   --------------
   --  trim #1 --
   --------------
   function trim (US : Text) return Text is
   begin
      return SU.Trim (US, AS.Both);
   end trim;


   --------------
   --  trim #2 --
   --------------
   function trim (S : String) return String is
   begin
      return AS.Fixed.Trim (S, AS.Both);
   end trim;


   ---------------
   --  int2str  --
   ---------------
   function int2str  (A : Integer) return String
   is
      raw : constant String := A'Img;
      len : constant Natural := raw'Length;
   begin
      return raw (2 .. len);
   end int2str;


   ----------------
   --  int2text  --
   ----------------
   function int2text (A : Integer) return Text is
   begin
      return SUS (int2str (A));
   end int2text;


   ----------------
   --  bool2str  --
   ----------------
   function bool2str  (A : Boolean) return String is
   begin
      if A then
         return "true";
      end if;
      return "false";
   end bool2str;


   -----------------
   --  bool2text  --
   -----------------
   function bool2text (A : Boolean) return Text is
   begin
      return SUS (bool2str (A));
   end bool2text;


   --------------------
   --  contains  #1  --
   --------------------
   function contains (S : String; fragment : String) return Boolean is
   begin
      return (AS.Fixed.Index (Source => S, Pattern => fragment) > 0);
   end contains;


   --------------------
   --  contains  #2  --
   --------------------
   function contains (US : Text; fragment : String) return Boolean is
   begin
      return (SU.Index (Source => US, Pattern => fragment) > 0);
   end contains;


   --------------
   --  part_1  --
   --------------
   function part_1 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (S'First .. slash - 1);
   end part_1;


   --------------
   --  part_2  --
   --------------
   function part_2 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (slash + separator'Length .. S'Last);
   end part_2;


   ---------------
   --  replace  --
   ---------------
   function replace (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      focus     : constant Natural :=
                           AS.Fixed.Index (Source => S, Pattern => rejectstr);
      returnstr : String := S;
   begin
      if focus > 0 then
        returnstr (focus) := shiny;
      end if;
      return returnstr;
   end replace;


   ---------------
   --  zeropad  --
   ---------------
   function zeropad (N : Natural; places : Positive) return String
   is
      template : String (1 .. places) := (others => '0');
      myimage  : constant String := trim (N'Img);
      startpos : constant Integer := 1 + places - myimage'Length;
   begin
      if startpos < 1 then
         return myimage;
      else
         template (startpos .. places) := myimage;
         return template;
      end if;
   end zeropad;


   --------------
   --  len #1  --
   --------------
   function len (US : Text) return Natural is
   begin
      return SU.Length (US);
   end len;


   --------------
   --  len #2  --
   --------------
   function len (S : String) return Natural is
   begin
      return S'Length;
   end len;


   ------------------
   --  count_char  --
   ------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   ---------------------
   --  redact_quotes  --
   ---------------------
   function redact_quotes (sql : String) return String
   is
      --  This block will mask anything between quotes (single or double)
      --  These are considered to be literal and not suitable for binding
      type seeking is (none, single, double);
      redacted    : String := sql;
      seek_status : seeking := none;
      arrow       : Positive := 1;
   begin
      if IsBlank (sql) then
         return "";
      end if;
      loop
         case sql (arrow) is
            when ''' =>
               case seek_status is
                  when none =>
                     seek_status := single;
                     redacted (arrow) := '#';
                  when single =>
                     seek_status := none;
                     redacted (arrow) := '#';
                  when double => null;
               end case;
            when ASCII.Quotation =>
               case seek_status is
                  when none =>
                     seek_status := double;
                     redacted (arrow) := '#';
                  when double =>
                     seek_status := none;
                     redacted (arrow) := '#';
                  when single => null;
               end case;
            when others => null;
         end case;
         exit when arrow = sql'Length;
         arrow := arrow + 1;
      end loop;
      return redacted;
   end redact_quotes;


   ----------------
   --  trim_sql  --
   ----------------
   function trim_sql (sql : String) return String is
      pass1 : String := trim (sql);
      pass2 : String (1 .. pass1'Length) := pass1;
   begin
      if pass2 (pass2'Last) = ASCII.Semicolon then
         return pass2 (1 .. pass2'Length - 1);
      else
         return pass2;
      end if;
   end trim_sql;


   ---------------------
   --  count_queries  --
   ---------------------
   function count_queries (trimmed_sql : String) return Natural
   is
      mask : String := redact_quotes (trimmed_sql);
   begin
      return count_char (S => mask, focus => ASCII.Semicolon) + 1;
   end count_queries;


   ----------------
   --  subquery  --
   ----------------
   function subquery (trimmed_sql : String; index : Positive) return String
   is
      mask     : String := redact_quotes (trimmed_sql);
      start    : Natural := trimmed_sql'First;
      segment  : Natural := 1;
      scanning : Boolean := (index = segment);
   begin
      for x in mask'Range loop
         if mask (x) = ASCII.Semicolon then
            if scanning then
               return trimmed_sql (start .. x - 1);
            else
               segment  := segment + 1;
               start    := x + 1;
               scanning := (index = segment);
            end if;
         end if;
      end loop;

      --  Here we're either scanning (return current segment) or we aren't,
      --  meaning the index was too high, so return nothing.  (This should
      --  never happen because caller knows how many segments there are and
      --  thus would not request something impossible like this.)
      if scanning then
         return trim (trimmed_sql (start .. trimmed_sql'Last));
      else
         return "";
      end if;
   end subquery;


   ---------------------
   --  num_set_items  --
   ---------------------
   function num_set_items (nv : String) return Natural
   is
      result : Natural := 0;
   begin
      if not IsBlank (nv) then
         result := 1;
         for x in nv'Range loop
            if nv (x) = ',' then
               result := result + 1;
            end if;
         end loop;
      end if;
      return result;
   end num_set_items;


end CommonText;

--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded;

package CommonText is

   package SU  renames Ada.Strings.Unbounded;

   subtype Text is SU.Unbounded_String;
   subtype UTF8 is Ada.Strings.UTF_Encoding.UTF_8_String;

   blank : constant Text := SU.Null_Unbounded_String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  converters : UTF8 <==> String
   function UTF8S (S8 : UTF8)   return String;
   function SUTF8 (S  : String) return UTF8;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;
   function equivalent (A : Text; B : String) return Boolean;

   --  Trim both sides
   function trim (US : Text) return Text;
   function trim (S : String) return String;

   --  unpadded numeric image
   function int2str  (A : Integer) return String;
   function int2text (A : Integer) return Text;

   --  convert boolean to lowercase string
   function bool2str  (A : Boolean) return String;
   function bool2text (A : Boolean) return Text;

   --  shorthand for index
   function contains (S : String; fragment : String) return Boolean;
   function contains (US : Text; fragment : String) return Boolean;

   --  Return half of a string split by separator
   function part_1 (S : String; separator : String := "/") return String;
   function part_2 (S : String; separator : String := "/") return String;

   --  Replace a single character with another single character (first found)
   function replace (S : String; reject, shiny : Character) return String;

   --  Numeric image with left-padded zeros
   function zeropad (N : Natural; places : Positive) return String;

   --  Returns length of string
   function len (US : Text) return Natural;
   function len (S : String) return Natural;

   --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Provides a mask of the given sql string, all quoted text set to '#'
   --  including the quote marks themselves.
   function redact_quotes (sql : String) return String;

   --  Removes leading and trailing whitespace, and any trailing semicolon
   function trim_sql (sql : String) return String;

   --  After masking, return number of queries separated by semicolons
   function count_queries (trimmed_sql : String) return Natural;

   --  Returns a single query given a multiquery and an index starting from 1
   function subquery (trimmed_sql : String; index : Positive) return String;

   --  With "set" imput of comma-separated values, return number of items
   function num_set_items (nv : String) return Natural;

end CommonText;

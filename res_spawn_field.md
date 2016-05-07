---
title: Spawn result fields
---

<div class="leftside">
<pre class="code">
package AdaBase is
   type field_types is (ft_nbyte0, ft_nbyte1, ft_nbyte2, ft_nbyte3, ft_nbyte4,
                        ft_nbyte8, ft_byte1, ft_byte2, ft_byte3, ft_byte4,
                        ft_byte8, ft_real9, ft_real18, ft_textual,
                        ft_widetext, ft_supertext, ft_timestamp,
                        ft_chain, ft_enumtype, ft_settype);
end AdaBase;

package AdaBase.Results.Field is

   type std_field is tagged private;
   type field_access is access std_field;

   type variant (datatype : field_types := ft_nbyte8) is
      record
         case datatype is
            when ft_nbyte0    => v00 : Boolean;
            when ft_nbyte1    => v01 : nbyte1;
            when ft_nbyte2    => v02 : nbyte2;
            when ft_nbyte3    => v03 : nbyte3;
            when ft_nbyte4    => v04 : nbyte4;
            when ft_nbyte8    => v05 : nbyte8;
            when ft_byte1     => v06 : byte1;
            when ft_byte2     => v07 : byte2;
            when ft_byte3     => v08 : byte3;
            when ft_byte4     => v09 : byte4;
            when ft_byte8     => v10 : byte8;
            when ft_real9     => v11 : real9;
            when ft_real18    => v12 : real18;
            when ft_textual   => v13 : textual;
            when ft_widetext  => v14 : textwide;
            when ft_supertext => v15 : textsuper;
            when ft_timestamp => v16 : AC.Time;
            when ft_chain     => v17 : chain_access;
            when ft_enumtype  => v18 : enumtype;
            when ft_settype   => v19 : settype_access;
         end case;
      end record;

   function spawn_field (data : variant; null_data : Boolean := False)
                         return field_access;
   function spawn_field (binob : chain) return field_access;
   function spawn_field (enumset : settype) return field_access;

end AdaBase.Results.Field;
</pre>
<h3>AdaBase.Results.Field.field_access function <br/>
spawn_field (see package, x3)</h3>
<p>
The spawn_field functions are low-level functions that are not typically
used in normal applicatons.  They create std_field variables dynamically and
are more typically used for testing.
</p>

<pre class="code">
with AdaBase.Results.Field;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Wide_Text_IO;

procedure Spawn_Fields is
   package TIO renames Ada.Text_IO;
   package WIO renames Ada.Wide_Text_IO;
   package IIO renames Ada.Integer_Text_IO;
   package SU  renames Ada.Strings.Unbounded;
   package ARC renames AdaBase.Results;

   FA : ARC.Field.field_access :=
        ARC.Field.spawn_field (binob => (50, 15, 4, 8));

   BR : ARC.Field.field_access :=
        ARC.Field.spawn_field (data =>
        (datatype => AdaBase.ft_textual,
              v13 => SU.To_Unbounded_String ("Baltimore Ravens")));

   myset : ARC.settype (1 .. 3) :=
               ((SU.To_Unbounded_String ("hockey"), 0),
                (SU.To_Unbounded_String ("baseball"), 3),
                (SU.To_Unbounded_String ("tennis"), 5));

   ST : ARC.Field.field_access := ARC.Field.spawn_field (enumset => myset);

   chain_len : Natural := FA.as_chain'Length;

begin
   TIO.Put_Line ("Chain #1 length:" & chain_len'Img);
   TIO.Put_Line ("Chain #1   type: " & FA.native_type'Img);
   for x in 1 .. chain_len loop
      TIO.Put ("  block" & x'Img & " value:" & FA.as_chain (x)'Img);
      IIO.Put (Item => Natural (FA.as_chain (x)), Base => 16);
      TIO.Put_Line ("");
   end loop;
   TIO.Put ("Chain #1 converted to 4-byte unsigned integer:" &
             FA.as_nbyte4'Img & "  ");
   IIO.Put (Item => Natural (FA.as_nbyte4), Base => 16);
   TIO.Put_Line ("");
   TIO.Put_Line ("");

   WIO.Put_Line ("Convert BR field to wide string: " & BR.as_wstring);
   TIO.Put_Line ("Convert ST settype to string:    " & ST.as_string);
   TIO.Put_Line ("Length of ST set:               " & myset'Length'Img);
end Spawn_Fields;
</pre>
<p class="caption">Example code: testcases/spawn_fields/spawn_fields.adb</p>
<br/>
<pre class="output">
Chain #1 length: 4
Chain #1   type: FT_CHAIN
  block 1 value: 50     16#32#
  block 2 value: 15      16#F#
  block 3 value: 4      16#4#
  block 4 value: 8      16#8#
Chain #1 converted to 4-byte unsigned integer: 134483762  16#8040F32#

Convert BR field to wide string: Baltimore Ravens
Convert ST settype to string:    hockey,baseball,tennis
Length of ST set:                3
</pre>
<p class="caption">Output using MySQL Driver</p>
<br/>
</div>
<div class="sidenav">
  <h3>See Also</h3>
  <ul>
    <li>{{ page.res_std_field }}</li>
  </ul>
</div>
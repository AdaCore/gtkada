------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--
--  This package provides a collection of "standard" pixmaps
--
--  </description>

with Gtkada.Types;

package Gtkada.Pixmaps is

   function "+" (Str : String) return Gtkada.Types.Chars_Ptr
     renames Gtkada.Types.New_String;

   Warning_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a yellow exclamation point
     (+"32 32 7 1",
      --  colors
      +". c #000000",
      +"# c #808000",
      +"a c #000080",
      +"b c none g none m none s Background",
      +"c c #808080",
      +"d c #f8fc00",
      +"e c #f8fcf8",
      --  pixels
      +"bbbbbbbbbbbbb###bbbbbbbbbbbbbbbb",
      +"bbbbbbbbbbbb#ddb.bbbbbbbbbbbbbbb",
      +"bbbbbbbbbbb#ddddb.cbbbbbbbbbbbbb",
      +"bbbbbbbbbbb#ddddd.ccbbbbbbbbbbbb",
      +"bbbbbbbbbb#ddddddb.ccbbbbbbbbbbb",
      +"bbbbbbbbbb#ddddddd.ccbbbbbbbbbbb",
      +"bbbbbbbbb#ddddddddb.ccbbbbbbbbbb",
      +"bbbbbbbbb#ddddddddd.ccbbbbbbbbbb",
      +"bbbbbbbb#ddddddddddb.ccbbbbbbbbb",
      +"bbbbbbbb#dddb...bddd.ccbbbbbbbbb",
      +"bbbbbbb#dddd.....dddb.ccbbbbbbbb",
      +"bbbbbbb#dddd.....dddd.ccbbbbbbbb",
      +"bbbbbb#ddddd.....ddddb.ccbbbbbbb",
      +"bbbbbb#ddddd.....ddddd.ccbbbbbbb",
      +"bbbbb#dddddd.....dddddb.ccbbbbbb",
      +"bbbbb#dddddd#...#dddddd.ccbbbbbb",
      +"bbbb#dddddddb...bddddddb.ccbbbbb",
      +"bbbb#dddddddd...dddddddd.ccbbbbb",
      +"bbb#ddddddddd#.#ddddddddb.ccbbbb",
      +"bbb#dddddddddb.bddddddddd.ccbbbb",
      +"bb#ddddddddddd.ddddddddddb.ccbbb",
      +"bb#ddddddddddddddddddddddd.ccbbb",
      +"b#dddddddddddb..bdddddddddb.ccbb",
      +"b#ddddddddddd....dddddddddd.ccbb",
      +"#dddddddddddd....ddddddddddb.ccb",
      +"#ddddddddddddb..bddddddddddd.ccb",
      +"#ddddddddddddddddddddddddddd.ccc",
      +"#ddddddddddddddddddddddddddb.ccc",
      +"b#ddddddddddddddddddddddddb.cccc",
      +"bb#........................ccccc",
      +"bbbbcccccccccccccccccccccccccccb",
      +"bbbbbcccccccccccccccccccccccccbb");

   Error_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a red stop sign
     (+"32 32 7 1",
      --  colors
      +". c #000000",
      +"# c #800000",
      +"a c #000080",
      +"b c none g none m none s Background",
      +"c c #808080",
      +"d c #f80000",
      +"e c #f8fcf8",
      --  pixels
      +"bbbbbbbbbbb########bbbbbbbbbbbbb",
      +"bbbbbbbb###dddddddd###bbbbbbbbbb",
      +"bbbbbbb#dddddddddddddd#bbbbbbbbb",
      +"bbbbb##dddddddddddddddd##bbbbbbb",
      +"bbbb#dddddddddddddddddddd#bbbbbb",
      +"bbb#dddddddddddddddddddddd#bbbbb",
      +"bbb#dddddddddddddddddddddd#cbbbb",
      +"bb#ddddddeddddddddddedddddd#cbbb",
      +"b#ddddddeeeddddddddeeedddddd#bbb",
      +"b#dddddeeeeeddddddeeeeeddddd#cbb",
      +"b#ddddddeeeeeddddeeeeedddddd#ccb",
      +"#ddddddddeeeeeddeeeeedddddddd#cb",
      +"#dddddddddeeeeeeeeeeddddddddd#cb",
      +"#ddddddddddeeeeeeeedddddddddd#cc",
      +"#dddddddddddeeeeeeddddddddddd#cc",
      +"#dddddddddddeeeeeeddddddddddd#cc",
      +"#ddddddddddeeeeeeeedddddddddd#cc",
      +"#dddddddddeeeeeeeeeeddddddddd#cc",
      +"#ddddddddeeeeeddeeeeedddddddd#cc",
      +"b#ddddddeeeeeddddeeeeedddddd#ccc",
      +"b#dddddeeeeeddddddeeeeeddddd#ccc",
      +"b#ddddddeeeddddddddeeedddddd#ccb",
      +"bb#ddddddeddddddddddedddddd#cccb",
      +"bbb#dddddddddddddddddddddd#ccccb",
      +"bbb#dddddddddddddddddddddd#cccbb",
      +"bbbb#dddddddddddddddddddd#cccbbb",
      +"bbbbb##dddddddddddddddd##ccccbbb",
      +"bbbbbbc#dddddddddddddd#cccccbbbb",
      +"bbbbbbbc###dddddddd###cccccbbbbb",
      +"bbbbbbbbbcc########ccccccbbbbbbb",
      +"bbbbbbbbbbccccccccccccccbbbbbbbb",
      +"bbbbbbbbbbbbbccccccccbbbbbbbbbbb");

   Information_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a blue "i" sign
     (+"32 32 6 1",
      --  colors
      +". c #000000",
      +"# c #000080",
      +"a c none g none m none s Background",
      +"b c #808080",
      +"c c #0000f8",
      +"d c #f8fcf8",
      --  pixels
      +"aaaaaaaaaaabbbbbbbbaaaaaaaaaaaaa",
      +"aaaaaaaabbbaddddddabbbaaaaaaaaaa",
      +"aaaaaabbaddddddddddddabbaaaaaaaa",
      +"aaaaabaddddddddddddddddabaaaaaaa",
      +"aaaabdddddddaccccaddddddd.aaaaaa",
      +"aaabddddddddccccccdddddddd.aaaaa",
      +"aabdddddddddccccccddddddddd.aaaa",
      +"abadddddddddaccccaddddddddda.aaa",
      +"abdddddddddddddddddddddddddd.baa",
      +"badddddddddddddddddddddddddda.ba",
      +"bddddddddddcccccccddddddddddd.ba",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"bddddddddddddcccccddddddddddd.bb",
      +"badddddddddddcccccdddddddddda.bb",
      +"abdddddddddddcccccdddddddddd.bbb",
      +"abaddddddddddcccccddddddddda.bbb",
      +"aabddddddddcccccccccddddddd.bbba",
      +"aaa.dddddddddddddddddddddd.bbbba",
      +"aaaa.dddddddddddddddddddd.bbbbaa",
      +"aaaaa.adddddddddddddddda.bbbbaaa",
      +"aaaaaa..adddddddddddda..bbbbaaaa",
      +"aaaaaaab...adddddda...bbbbbaaaaa",
      +"aaaaaaaabbb...addd.bbbbbbbaaaaaa",
      +"aaaaaaaaaabbbb.ddd.bbbbbaaaaaaaa",
      +"aaaaaaaaaaaaab.ddd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaa.dd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaa.d.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaa..bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaabbbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaa");

   Confirmation_Xpm : Gtkada.Types.Chars_Ptr_Array :=
   --  Provide a blue question mark
     (+"32 32 6 1",
      --  colors
      +". c #000000",
      +"# c #000080",
      +"a c none g none m none s Background",
      +"b c #808080",
      +"c c #0000f8",
      +"d c #f8fcf8",
      --  pixels
      +"aaaaaaaaaaabbbbbbbbaaaaaaaaaaaaa",
      +"aaaaaaaabbbaddddddabbbaaaaaaaaaa",
      +"aaaaaabbaddddddddddddabbaaaaaaaa",
      +"aaaaabaddddddddddddddddabaaaaaaa",
      +"aaaabdddddddddddddddddddd.aaaaaa",
      +"aaabdddddddaccccccaddddddd.aaaaa",
      +"aabdddddddacaddccccaddddddd.aaaa",
      +"abadddddddccddddccccddddddda.aaa",
      +"abddddddddccccddccccdddddddd.baa",
      +"baddddddddccccdaccccdddddddda.ba",
      +"bdddddddddaccadccccdddddddddd.ba",
      +"bdddddddddddddacccddddddddddd.bb",
      +"bdddddddddddddcccdddddddddddd.bb",
      +"bdddddddddddddccadddddddddddd.bb",
      +"bdddddddddddddccddddddddddddd.bb",
      +"badddddddddddddddddddddddddda.bb",
      +"abdddddddddddaccaddddddddddd.bbb",
      +"abaddddddddddccccdddddddddda.bbb",
      +"aabddddddddddccccdddddddddd.bbba",
      +"aaa.dddddddddaccaddddddddd.bbbba",
      +"aaaa.dddddddddddddddddddd.bbbbaa",
      +"aaaaa.adddddddddddddddda.bbbbaaa",
      +"aaaaaa..adddddddddddda..bbbbaaaa",
      +"aaaaaaab...adddddda...bbbbbaaaaa",
      +"aaaaaaaabbb...addd.bbbbbbbaaaaaa",
      +"aaaaaaaaaabbbb.ddd.bbbbbaaaaaaaa",
      +"aaaaaaaaaaaaab.ddd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaa.dd.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaa.d.bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaa..bbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaabbbaaaaaaaaaaa",
      +"aaaaaaaaaaaaaaaaaaabbaaaaaaaaaaa");

end Gtkada.Pixmaps;

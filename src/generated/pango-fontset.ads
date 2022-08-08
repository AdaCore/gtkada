------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  A Pango.Fontset.Pango_Fontset represents a set of Pango.Font.Pango_Font to
--  use when rendering text. It is the result of resolving a
--  Pango.Font.Pango_Font_Description against a particular
--  Pango.Context.Pango_Context. It has operations for finding the component
--  font for a particular Unicode character, and for finding a composite set of
--  metrics for the entire fontset.
--
--  </description>
--  <description>
--  an object containing a set of pango.Font objects
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Pango.Font;         use Pango.Font;
with Pango.Font_Metrics; use Pango.Font_Metrics;

package Pango.Fontset is

   type Pango_Fontset_Record is new GObject_Record with null record;
   type Pango_Fontset is access all Pango_Fontset_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Pango_Fontset_Foreach_Func is access function
     (Fontset : not null access Pango_Fontset_Record'Class;
      Font    : not null access Pango.Font.Pango_Font_Record'Class)
   return Boolean;
   --  A callback function used by Pango.Fontset.Foreach when enumerating the
   --  fonts in a fontset.
   --  Since: gtk+ 1.4
   --  "fontset": a Pango.Fontset.Pango_Fontset
   --  "font": a font from Fontset

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_fontset_get_type");

   -------------
   -- Methods --
   -------------

   procedure Foreach
      (Self : not null access Pango_Fontset_Record;
       Func : Pango_Fontset_Foreach_Func);
   --  Iterates through all the fonts in a fontset, calling Func for each one.
   --  If Func returns True, that stops the iteration.
   --  Since: gtk+ 1.4
   --  "func": Callback function

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Foreach_User_Data is

      type Pango_Fontset_Foreach_Func is access function
        (Fontset   : not null access Pango.Fontset.Pango_Fontset_Record'Class;
         Font      : not null access Pango.Font.Pango_Font_Record'Class;
         User_Data : User_Data_Type) return Boolean;
      --  A callback function used by Pango.Fontset.Foreach when enumerating the
      --  fonts in a fontset.
      --  Since: gtk+ 1.4
      --  "fontset": a Pango.Fontset.Pango_Fontset
      --  "font": a font from Fontset
      --  "user_data": callback data

      procedure Foreach
         (Self : not null access Pango.Fontset.Pango_Fontset_Record'Class;
          Func : Pango_Fontset_Foreach_Func;
          Data : User_Data_Type);
      --  Iterates through all the fonts in a fontset, calling Func for each
      --  one. If Func returns True, that stops the iteration.
      --  Since: gtk+ 1.4
      --  "func": Callback function
      --  "data": data to pass to the callback function

   end Foreach_User_Data;

   function Get_Font
      (Self : not null access Pango_Fontset_Record;
       Wc   : Guint) return Pango.Font.Pango_Font;
   --  Returns the font in the fontset that contains the best glyph for the
   --  Unicode character Wc.
   --  "wc": a Unicode character

   function Get_Metrics
      (Self : not null access Pango_Fontset_Record)
       return Pango.Font_Metrics.Pango_Font_Metrics;
   --  Get overall metric information for the fonts in the fontset.

end Pango.Fontset;

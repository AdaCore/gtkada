------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
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

with Cairo;               use Cairo;
with Gdk.RGBA;            use Gdk.RGBA;
with Glib;                use Glib;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Style;        use Gtkada.Style;
with GNAT.Strings;        use GNAT.Strings;
with Pango.Cairo;         use Pango.Cairo;
with Pango.Font;          use Pango.Font;
with Pango.Layout;        use Pango.Layout;

package body Create_Canvas_Links is

   Width  : constant Gdouble := 13.0;
   Height : constant Gdouble := 13.0;

   type Demo_Item_Record is new Canvas_Item_Record with record
      Style : Drawing_Style;
      Text  : String_Access;
   end record;
   type Demo_Item is access all Demo_Item_Record'Class;
   overriding procedure Draw
     (Self : access Demo_Item_Record; Cr : Cairo_Context);

   Layout : Pango_Layout;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows the various types of routing algorithms for"
        & " links in a @bGtkAda.Canvas@B."
        & ASCII.LF
        & "The links can be attached to any side of the source and target"
        & " objects, or left to automatically chose the side to get a shorter"
        & " link. The text in each pair of items indicates what sides the link"
        & " is attached to: A=auto, R=right, L=left, T=top, B=bottom."
        & ASCII.LF
        & "On each line, the items are grouped by pair, where the second item"
        & " occupies each of the height positions around the first item."
        & ASCII.LF
        & "The links can chose among several automatic routing algorithms,"
        & " using either straight lines or Bezier curves, and connecting the"
        & " source and target in the most direct way or limiting to horizontal"
        & " and vertical links."
        & ASCII.LF
        & "Finally, links can use a variety of colors, dashed lins,"
        & " widths,...";
   end Help;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : access Demo_Item_Record; Cr : Cairo_Context)
   is
   begin
      Self.Style.Draw_Rect (Cr, (0.0, 0.0), Width, Height);

      if Self.Text /= null then
         Layout.Set_Text (Self.Text.all);
         Pango.Cairo.Show_Layout (Cr, Layout);
      end if;
   end Draw;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      M : constant Gdouble := 22.0;  --  margin between items
      W : constant Gdouble := Width + M;
      H : constant Gdouble := Height + M;

      PW : constant Gdouble := W * 2.0;  --  space between groups of items

      Canvas       : Interactive_Canvas;
      Scrolled     : Gtk_Scrolled_Window;
      Object_Style : Drawing_Style;
      Link_Style   : Drawing_Style;
      Link         : Canvas_Link;
      It           : Integer;
      Y, X         : Gdouble;

      Pos          : constant Point_Array :=
        (
          --  Second item to the left and above
          (W,     H),
          (0.0,   0.0),

          --  Second item exactly above
          (PW,     H),
          (PW,     0.0),

          --  Second item to the right and above
          (PW + W,      H),
          (PW + W * 2.0,  0.0),

          --  Second item to the left and same y
          (W,     H * 2.0),
          (0.0,   H * 2.0),

          --  Second item to the right and same y
          (PW + W,        H * 2.0),
          (PW + W * 2.0,  H * 2.0),

          --  Second item to the left and below
          (W,     H * 3.0),
          (0.0,   H * 3.0 + H),

          --  Second item exactly below
          (PW,     H * 3.0),
          (PW,     H * 3.0 + H),

          --  Second item to the right and below
          (PW + W,        H * 3.0),
          (PW + W * 2.0,  H * 3.0 + H)
         );


      Items        : array (Pos'Range) of Demo_Item;

   begin
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Layout := Frame.Create_Pango_Layout;
      Layout.Set_Font_Description (From_String ("sans 8px"));

      Gtk_New (Canvas);
      Scrolled.Add (Canvas);

      Object_Style := Gtk_New
        (Stroke => Black_RGBA,
         Fill   => Create_Rgba_Pattern
           ((1.0, 1.0, 1.0, 0.8)));
      Link_Style := Gtk_New (Stroke => Black_RGBA);

      Y := 0.0;
      X := 0.0;

      for Route in Route_Style loop
         for From_Side in Side_Attachment loop
            for To_Side in Side_Attachment loop
               for J in Items'Range loop
                  Items (J) := new Demo_Item_Record;
                  Items (J).Style := Object_Style;

                  if J mod 2 = 0 then
                     declare
                        F : constant String :=
                          Side_Attachment'Image (From_Side);
                        T : constant String :=
                          Side_Attachment'Image (To_Side);
                     begin
                        Items (J).Text := new String'
                          (F (F'First) & T (T'First));
                     end;
                  end if;

                  Items (J).Set_Screen_Size (Width, Height);
                  Canvas.Put (Items (J), X + Pos (J).X, Y + Pos (J).Y);
               end loop;

               It := Items'First;
               while It < Items'Last loop
                  Link := new Canvas_Link_Record;
                  Link.Configure (Link_Style, Routing => Route);
                  Link.Set_Src_Pos ((0.5, 0.5, From_Side));
                  Link.Set_Dest_Pos ((0.5, 0.5, To_Side));
                  Canvas.Add_Link (Link, Items (It), Items (It + 1));
                  It := It + 2;
               end loop;

               X := X + PW + W * 3.0 + 20.0;
            end loop;

            X := 0.0;
            Y := Y + H * 4.0 + 60.0;
         end loop;
      end loop;

      Frame.Show_All;
   end Run;

end Create_Canvas_Links;

-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;
with Gtk.Object; use Gtk.Object;
with Gtk.Container; use Gtk.Container;
with Gtk.Notebook; use Gtk.Notebook;

package body Gtk.Label is

   ---------
   -- Get --
   ---------

   function Get (Label : access Gtk_Label_Record) return String is
      procedure Internal (Label : in     System.Address;
                          Str   :    out C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_get");
      Temp : chars_ptr;
   begin
      Internal (Get_Object (Label), Temp);
      return Value (Temp);
   end Get;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String) is
   begin
      Label := new Gtk_Label_Record;
      Initialize (Label, Str);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Label : access Gtk_Label_Record'Class;
                         Str   : in     String) is
      function Internal (Str : in String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");
   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
      Initialize_User_Data (Label);
   end Initialize;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify (Label : access Gtk_Label_Record;
                          Jtype : in Enums.Gtk_Justification) is
      procedure Internal (Label : in System.Address;
                          Jtype : in Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");
   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Label : access Gtk_Label_Record;
                       Str   : in String) is
      procedure Internal (Label : in System.Address;
                          Str   : in String);
      pragma Import (C, Internal, "gtk_label_set_text");
   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Text;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern (Label   : access Gtk_Label_Record;
                          Pattern : in String)
   is
      procedure Internal (Label   : in System.Address;
                          Pattern : in String);
      pragma Import (C, Internal, "gtk_label_set_pattern");
   begin
      Internal (Get_Object (Label), Pattern & ASCII.NUL);
   end Set_Pattern;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record;
                            Wrap  : in Boolean)
   is
      procedure Internal (Label : in System.Address;
                          Wrap  : in Gint);
      pragma Import (C, Internal, "gtk_label_set_line_wrap");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Wrap));
   end Set_Line_Wrap;

   -----------------
   -- Parse_Uline --
   -----------------

   procedure Parse_Uline (Label : access Gtk_Label_Record;
                          Text  : in     String)
   is
      function Internal (Label : in System.Address;
                         Text  : in String)
                        return Guint;
      pragma Import (C, Internal, "gtk_label_parse_uline");
      Keyval : Guint;
   begin
      Keyval := Internal (Get_Object (Label), Text & ASCII.NUL);
   end Parse_Uline;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Child_Name : String_Ptr := Get_Field (N, "child_name");
      S          : String_Ptr;
      Top        : String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      P          : Node_Ptr;
      Page_Num   : Gint;

      function Adjust (S : String) return String;
      --  Replace non "compilable" characters (e.g ASCII.LF) and return
      --  a printable and "compilable" Ada string.

      function Adjust (S : String) return String is
         T : String (1 .. S'Length + 256);
         K : Natural := 1;

      begin
         for J in S'Range loop
            if S (J) = ASCII.LF then
               T (K .. K + 15) := """ & ASCII.LF & """;
               K := K + 16;

            --  Skip additional CR present on Win32
            elsif S (J) /= ASCII.CR then
               T (K) := S (J);
               K := K + 1;
            end if;
         end loop;

         return T (1 .. K - 1);
      end Adjust;

   begin
      Gen_New (N, "Label", Adjust (Get_Field (N, "label").all),
        File => File, Delim => '"');
      Misc.Generate (N, File);
      Gen_Set (N, "Label", "justify", File);
      Gen_Set (N, "Label", "Line_Wrap", "wrap", File);

      if Child_Name /= null then
         if Get_Part (Child_Name.all, 2) = "tab" then
            P := N.Parent.Child;
            Page_Num := 0;

            while P /= N loop
               S := Get_Field (P, "child_name");

               if S /= null and then S.all = Child_Name.all then
                  Page_Num := Page_Num + 1;
               end if;

               P := P.Next;
            end loop;

            Put_Line (File,
              "   Notebook.Set_Tab (" &
              To_Ada (Top.all) & "." &
              To_Ada (Find_Tag
                (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                 "name").Value.all) & "," &
              Gint'Image (Page_Num) & ", " &
              To_Ada (Top.all) & "." &
              To_Ada (Get_Field (N, "name").all) & ");");
         end if;

      elsif not N.Specific_Data.Has_Container then
         Gen_Call_Child (N, null, "Container", "Add", File => File);
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

   procedure Generate (Label : in out Gtk.Object.Gtk_Object;
                       N     : in Node_Ptr) is
      Child_Name : String_Ptr := Get_Field (N, "child_name");
      S          : String_Ptr;
      P          : Node_Ptr;
      Page_Num   : Gint;

   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Label (Label), Get_Field (N, "label").all);
         Set_Object (Get_Field (N, "name"), Label);
         N.Specific_Data.Created := True;
      end if;

      Misc.Generate (Label, N);

      S := Get_Field (N, "justify");

      if S /= null then
         Set_Justify (Gtk_Label (Label),
           Enums.Gtk_Justification'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "wrap");

      if S /= null then
         Set_Line_Wrap (Gtk_Label (Label), Boolean'Value (S.all));
      end if;

      if Child_Name /= null then
         if Get_Part (Child_Name.all, 2) = "tab" then
            P := N.Parent.Child;
            Page_Num := 0;

            while P /= N loop
               S := Get_Field (P, "child_name");

               if S /= null and then S.all = Child_Name.all then
                  Page_Num := Page_Num + 1;
               end if;

               P := P.Next;
            end loop;

            Set_Tab
              (Gtk_Notebook (Get_Object (Find_Tag
                 (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                  "name").Value)), Page_Num, Widget.Gtk_Widget (Label));
         end if;

      elsif not N.Specific_Data.Has_Container then
         Container.Add
           (Gtk_Container (Get_Object (Get_Field (N.Parent, "name"))),
            Widget.Gtk_Widget (Label));
         N.Specific_Data.Has_Container := True;
      end if;
   end Generate;

end Gtk.Label;

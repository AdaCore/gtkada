-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                    Copyright (C) 1999                             --
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

with Gtk.Alignment;
with Gtk.Arrow;
with Gtk.Aspect_Frame;
with Gtk.Box;
with Gtk.Button;
--  with Gtk.Button_Box;
with Gtk.Calendar;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;
--  with Gtk.Clist;
--  with Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;
with Gtk.Combo;
--  with Gtk.Ctree;
--  with Gtk.Curve;
with Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Editable;
with Gtk.Event_Box;
with Gtk.File_Selection;
--  with Gtk.Fixed;
--  with Gtk.Font_Selection;
with Gtk.Frame;
--  with Gtk.Gamma_Curve;
--  with Gtk.GEntry;
--  with Gtk.Handle_Box;
--  with Gtk.Hbutton_Box;
--  with Gtk.Image;
with Gtk.Input_Dialog;
with Gtk.Item;
with Gtk.Label;
--  with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Notebook;
with Gtk.Option_Menu;
--  with Gtk.Paned;
--  with Gtk.Pixmap;
with Gtk.Preview;
with Gtk.Progress_Bar;
with Gtk.Radio_Button;
--  with Gtk.Radio_Menu_Item;
with Gtk.Ruler;
with Gtk.Scale;
with Gtk.Scrollbar;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Spin_Button;
with Gtk.Status_Bar;
with Gtk.Table;
with Gtk.Text;
--  with Gtk.Tips_Query;
with Gtk.Toggle_Button;
with Gtk.Toolbar;
--  with Gtk.Tooltips;
with Gtk.Tree;
with Gtk.Tree_Item;
--  with Gtk.VButton_Box;
--  with Gtk.Viewport;
with Gtk.Widget;
with Gtk.Window;

with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;
with GNAT.HTable;

package body Gtk.Glade is

   use Gtk;

   Max_Widgets : constant := 200;
   --  Maximum number of widgets

   subtype Hash_Header is Natural range 0 .. 300;
   --  Number of hash headers, related (for efficiency purposes only)
   --  to the maximum number of widgets.

   -----------------------
   --  Local functions  --
   -----------------------

   function Hash  (S : String_Ptr) return Hash_Header;
   function Equal (S1, S2 : String_Ptr) return Boolean;
   --  Hash and equality functions for hash table

   package SHT is new GNAT.HTable.Simple_HTable
     (Header_Num => Hash_Header,
      Element    => Generate_Rec,
      No_Element => Generate_Rec' (null, null),
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   procedure Create_Widget (N : Node_Ptr; Top_Level : Boolean := True);
   --  Create a widget and all its children from a given widget tree
   --  Top_Level is True when Create_Widget is called for a top level
   --  widget

   procedure Print_Create_Function
     (N : Node_Ptr; File : File_Type; First : Boolean := False);
   --  Print body of a given "create" function to file. First is used only
   --  internally when this procedure is called recursively.

   procedure Print_Header (N : Node_Ptr; File : File_Type);
   --  Print the main procedure with the name of the project contained in N
   --  to file.

   procedure Print_Foot_Page (N : Node_Ptr; File : File_Type);
   --  Print end of each "create" function to file.

   procedure Print_Var
     (N : Node_Ptr; File : File_Type; First : Boolean := True);
   --  Print variable declarations for a given "create" function to file.
   --  First is only used internally.

   -----------
   -- Equal --
   -----------

   function Equal (S1, S2 : String_Ptr) return Boolean is
   begin
      return S1.all = S2.all;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (S : String_Ptr) return Hash_Header is
      N : Natural := 0;

   begin
      --  Add up characters of name, mod our table size

      for J in S'Range loop
         N := (N + Character'Pos (S (J))) mod (Hash_Header'Last + 1);
      end loop;

      return N;
   end Hash;

   -------------------
   -- Create_Widget --
   -------------------

   procedure Create_Widget (N : Node_Ptr; Top_Level : Boolean := True) is
      P      : Node_Ptr;
      Object : Gtk.Object.Gtk_Object;

      use type Gtk.Object.Gtk_Object;
   begin
      Get_Dgate (Get_Field (N, "class").all) (Object, N);
      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            Create_Widget (P, False);
         end if;

         P := P.Next;
      end loop;

      if Top_Level and then Object /= null then
         Widget.Show_All (Widget.Gtk_Widget (Object));
      end if;
   end Create_Widget;

   -----------------
   -- Generic_Ptr --
   -----------------

   procedure Generic_Ptr (N : Node_Ptr; File : File_Type) is
      S : String_Ptr := Get_Field (N, "name");

   begin
      if S /= null then
         Put_Line (File, "   --  WARNING: Unsupported widget " &
           Get_Field (N, "class").all & " (" & S.all & ")");
      end if;
   exception
      when Constraint_Error =>
         null;
   end Generic_Ptr;
 
   ------------------
   -- Generic_DPtr --
   ------------------

   procedure Generic_DPtr
     (Object : in out Gtk.Object.Gtk_Object; N : Node_Ptr)
   is
      S : String_Ptr := Get_Field (N, "name");

   begin
      if S /= null then
         New_Line;
         Put_Line ("GtkAda-WARNING *: Unsupported widget " &
           Get_Field (N, "class").all & " (" & S.all & ")");
      end if;
   exception
      when Constraint_Error =>
         null;
   end Generic_DPtr;

   ---------------------------
   -- Print_Create_Function --
   ---------------------------

   procedure Print_Create_Function
     (N : Node_Ptr; File : File_Type; First : Boolean := False)
   is
      P : Node_Ptr;
      S : String_Ptr;

   begin
      S := Get_Field (N, "class");
      Get_Gate (S.all) (N, File);

      if not First and then S.all /= "Placeholder" then
         New_Line (File);
      end if;

      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            Print_Create_Function (P, File);
         end if;

         P := P.Next;
      end loop;
   end Print_Create_Function;

   ---------------------
   -- Print_Foot_Page --
   ---------------------

   procedure Print_Foot_Page (N : Node_Ptr; File : File_Type) is
      Name : String := To_Ada (Get_Field (N, "name").all);

   begin
      Put_Line (File, "   return " & Name & ";");
      Put_Line (File, "end Create_" & Name & ";");
      New_Line (File);
   end Print_Foot_Page;

   ------------------
   -- Print_Header --
   ------------------

   procedure Print_Header (N : Node_Ptr; File : File_Type) is
      P, Q, R : String_Ptr;
      M : Node_Ptr;

   begin
      P := Get_Field (N.Child.Next, "name");
      R := Get_Field (N.Child, "name");

      Put_Line ("with Gtk; use Gtk;");
      Put_Line ("with Gtk.Main;");
      Put_Line ("with Gtk.Widget; use Gtk.Widget;");

      M := N.Child.Next;

      loop
         exit when M = null;

         P := Get_Field (M, "name");
         Q := Get_Field (M, "class");

         if P /= null and Q /= null then
            Put_Line ("with Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) &
              "; use Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) & ";");
            Put_Line ("with Create_" & To_Ada (P.all) & ";");
         end if;

         M := M.Next;
      end loop;

      New_Line;
      Put_Line ("procedure " & To_Ada (R.all) & " is");
      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");
         Q := Get_Field (M, "class");

         if P /= null and Q /= null then
            Put_Line ("   " & To_Ada (P.all) & " : " & To_Ada (Q.all) & ";");
         end if;

         M := M.Next;
      end loop;

      New_Line;
      Put_Line ("begin");
      Put_Line ("   Gtk.Main.Set_Locale;");
      Put_Line ("   Gtk.Main.Init;");
      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");

         if P /= null then
            Put_Line ("   " & To_Ada (P.all) & " := Create_" &
              To_Ada (P.all) & ";");
            Put_Line ("   Widget.Show_All (Gtk_Widget (" &
              To_Ada (P.all) & "));");
         end if;

         M := M.Next;
      end loop;

      Put_Line ("   Gtk.Main.Main;");
      Put_Line ("end " & To_Ada (R.all) & ";");
      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");
         Q := Get_Field (M, "class");

         if P /= null and Q /= null then
            New_Line;
            Put_Line ("with Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) &
              "; use Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) & ";");
            New_Line;
            Put_Line ("function Create_" & To_Ada (P.all) & " return " &
              To_Ada (Q.all) & ";");
         end if;

         M := M.Next;
      end loop;
   end Print_Header;

   ---------------
   -- Print_Var --
   ---------------

   procedure Print_Var
     (N : Node_Ptr; File : File_Type; First : Boolean := True)
   is
      P : Node_Ptr := N;
      Q : Node_Ptr;
      S : String_Ptr;
      First_Option_Menu : Boolean := True;

   begin
      while P /= null loop
         if P.Tag.all = "widget" then
            Q := Find_Tag (P.Child, "name");

            if Q /= null then
               S := Get_Field (P, "class");
               Put_Line (File, "   " & To_Ada (Q.Value.all) & " : " &
                 To_Ada (S.all) & ";");

               --  Special cases:
               --  Declare a Gtk_Adjustment with each Gtk_Spin_Button,
               --  Gtk_Scale and Gtk_Scrollbar

               if S.all = "GtkSpinButton" or else S.all = "GtkHScale"
                 or else S.all = "GtkVScale"
                 or else S.all = "GtkHScrollbar"
                 or else S.all = "GtkVScrollbar"
               then
                  Put_Line (File, "   " & To_Ada (Q.Value.all) &
                    "_Adj : Gtk_Adjustment;");
               end if;

               --  Declare a GSList with each Widget containing radio buttons

               if S.all = "GtkRadioButton" then
                  if not P.Parent.Specific_Data.Has_Radio_Button_Group then
                     Put_Line (File, "   " &
                       To_Ada (Get_Field (P.Parent, "name").all) &
                       "_Group : Widget_SList.GSList;");
                     P.Parent.Specific_Data.Has_Radio_Button_Group := True;
                  end if;
               end if;

               --  Declare a Glist with each combo box

               if S.all = "GtkCombo" then
                  Put_Line (File, "   " &
                    To_Ada (Get_Field (P.Parent, "name").all) &
                    "_Items : String_List.Glist;");
               end if;

               --  Declare a menu with each option menu

               if S.all = "GtkOptionMenu" then
                  Put_Line (File, "   " &
                    To_Ada (Get_Field (P.Parent, "name").all) &
                    "_Menu : Gtk_Menu;");

                  if First_Option_Menu then
                     Put_Line (File, "   Menu_Item : Gtk_Menu_Item;");
                     First_Option_Menu := False;
                  end if;
               end if;

               Print_Var (P.Child, File, False);
            end if;
         end if;

         exit when First;

         P := P.Next;
      end loop;
   end Print_Var;

   ------------------------
   --  Global functions  --
   ------------------------

   --------------
   -- Generate --
   --------------

   procedure Generate (File : String) is
   begin
      Generate (Parse (File));
   end Generate;

   procedure Generate (N : Node_Ptr) is
      M           : Node_Ptr;
      Buffer      : String (1 .. 256);
      Len         : Natural;
      Num_Signals : Natural;
      Output      : File_Type;
      Project     : String :=
        To_Ada (Get_Field (Find_Tag (N.Child, "project"), "name").all);
      Name        : String_Ptr;
      Class       : String_Ptr;

   begin
      Print_Header (N, Output);
      M := N.Child.Next;

      loop
         exit when M = null;

         Name  := Get_Field (M, "name");
         Class := Get_Field (M, "class");

         if Name /= null and Class /= null then
            Create (Output);
            Put_Line (Output, "function Create_" &
              To_Ada (Name.all) & " return " & To_Ada (Class.all) & " is");
            Put_Line (Output, "   Cb_Id : Glib.Guint;");
            Put_Line (Output, "   The_Accel_Group : Gtk_Accel_Group;");
            Print_Var (M, Output);
            New_Line (Output);
            Put_Line (Output, "begin");
            Print_Create_Function (M, Output, True);
            Print_Foot_Page (M, Output);
            New_Line;
            Put_Line ("with Glib;");
            Put_Line ("with Gtk; use Gtk;");

            --  ??? It would be nice to determine when these packages are
            --  needed

            Put_Line ("with Gdk.Types; use Gdk.Types;");
            Put_Line ("with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;");
            Put_Line ("with Gtk.Enums;  use Gtk.Enums;");
            Put_Line ("with Gtk.Button; use Gtk.Button;");
            Put_Line ("with Gtk.Widget; use Gtk.Widget;");
            Put_Line ("with Gtk.Window; use Gtk.Window;");
            Put_Line ("with Gtk.Pixmap; use Gtk.Pixmap;");
            Put_Line ("with Gtk.Accel_Group; use Gtk.Accel_Group;");

            Put_Line ("with Callbacks_" & Project &
              "; use Callbacks_" & Project & ";");
            Gen_Packages (Standard_Output);
            Reset_Packages;
            New_Line;
            Reset (Output, In_File);

            while not End_Of_File (Output) loop
               Get_Line (Output, Buffer, Len);

               if Len < Buffer'Length then
                  Put_Line (Buffer (1 .. Len));
               else
                  Put (Buffer);
               end if;
            end loop;

            Delete (Output);
         end if;

         M := M.Next;
      end loop;

      New_Line;
      Num_Signals := Gen_Signal_Instanciations (Project, Standard_Output);
   end Generate;

   -----------------
   -- Instanciate --
   -----------------

   procedure Instanciate (File : String) is
   begin
      Instanciate (Parse (File));
   end Instanciate;

   procedure Instanciate (N : Node_Ptr) is
      P : Node_Ptr;
   begin
      P := N.Child.Next;

      loop
         exit when P = null;

         if Get_Field (P, "class") /= null then
            Create_Widget (P);
         end if;

         P := P.Next;
      end loop;
   end Instanciate;

   --------------
   -- Get_Gate --
   --------------

   function Get_Gate (Class : String) return Generate_Ptr is
      S   : aliased String := Class;
      Ptr : Generate_Ptr;
   begin
      Ptr := SHT.Get (S'Unchecked_Access).Gate;

      if Ptr = null then
         return Generic_Ptr'Access;
      else
         return Ptr;
      end if;
   end Get_Gate;

   ---------------
   -- Get_Dgate --
   ---------------

   function Get_Dgate (Class : String) return Dynamic_Generate_Ptr is
      S   : aliased String := Class;
      Ptr : Dynamic_Generate_Ptr;
   begin
      Ptr := SHT.Get (S'Unchecked_Access).Dgate;

      if Ptr = null then
         return Generic_DPtr'Access;
      else
         return Ptr;
      end if;
   end Get_Dgate;

begin
   SHT.Set (new String '("GtkAlignment"),
     (Gtk.Alignment.Generate'Access, Gtk.Alignment.Generate'Access));
   SHT.Set (new String '("GtkArrow"),
     (Gtk.Arrow.Generate'Access, Gtk.Arrow.Generate'Access));
   SHT.Set (new String '("GtkAspectFrame"),
     (Gtk.Aspect_Frame.Generate'Access, Gtk.Aspect_Frame.Generate'Access));
   SHT.Set (new String '("GtkBox"),
     (Gtk.Box.Generate'Access, Gtk.Box.Generate'Access));
   SHT.Set (new String '("GtkHBox"),
     (Gtk.Box.Generate'Access, Gtk.Box.Generate'Access));
   SHT.Set (new String '("GtkVBox"),
     (Gtk.Box.Generate'Access, Gtk.Box.Generate'Access));
   SHT.Set (new String '("GtkButton"),
     (Gtk.Button.Generate'Access, Gtk.Button.Generate'Access));
   --  SHT.Set (new String '("GtkButtonBox"),
   --    (Gtk.Button_Box.Generate'Access, Gtk.Button_Box.Generate'Access));
   SHT.Set (new String '("GtkCalendar"),
     (Gtk.Calendar.Generate'Access, Gtk.Calendar.Generate'Access));
   SHT.Set (new String '("GtkCheckButton"),
     (Gtk.Check_Button.Generate'Access, Gtk.Check_Button.Generate'Access));
   SHT.Set (new String '("GtkCheckMenuItem"),
     (Gtk.Check_Menu_Item.Generate'Access,
      Gtk.Check_Menu_Item.Generate'Access));
   --  SHT.Set (new String '("GtkCList"),
   --    (Gtk.Clist.Generate'Access, Gtk.Clist.Generate'Access));
   --  SHT.Set (new String '("GtkColorSelection"),
   --    (Gtk.Color_Selection.Generate'Access,
   --     Gtk.Color_Selection.Generate'Access));
   SHT.Set (new String '("GtkColorSelectionDialog"),
     (Gtk.Color_Selection_Dialog.Generate'Access,
      Gtk.Color_Selection_Dialog.Generate'Access));
   SHT.Set (new String '("GtkCombo"),
     (Gtk.Combo.Generate'Access, Gtk.Combo.Generate'Access));
   --  SHT.Set (new String '("GtkCtree"),
   --    (Gtk.Ctree.Generate'Access, Gtk.Ctree.Generate'Access));
   --  SHT.Set (new String '("GtkCurve"),
   --    (Gtk.Curve.Generate'Access, Gtk.Curve.Generate'Access));
   SHT.Set (new String '("GtkDialog"),
     (Gtk.Dialog.Generate'Access, Gtk.Dialog.Generate'Access));
   SHT.Set (new String '("GtkDrawingArea"),
     (Gtk.Drawing_Area.Generate'Access, Gtk.Drawing_Area.Generate'Access));
   SHT.Set (new String '("GtkEditable"),
     (Gtk.Editable.Generate'Access, Gtk.Editable.Generate'Access));
   SHT.Set (new String '("GtkEventBox"),
     (Gtk.Event_Box.Generate'Access, Gtk.Event_Box.Generate'Access));
   SHT.Set (new String '("GtkFileSelection"),
     (Gtk.File_Selection.Generate'Access, Gtk.File_Selection.Generate'Access));
   --  SHT.Set (new String '("GtkFixed"),
   --    (Gtk.Fixed.Genrate'Access, Gtk.Fixed.Genrate'Access));
   --  SHT.Set (new String '("GtkFontSelection"),
   --    (Gtk.Font_Selection.Generate'Access,
   --     Gtk.Font_Selection.Generate'Access));
   SHT.Set (new String '("GtkFrame"),
     (Gtk.Frame.Generate'Access, Gtk.Frame.Generate'Access));
   --  SHT.Set (new String '("GtkGammaCurve"),
   --    (Gtk.Gamma_Curve.Generate'Access, Gtk.Gamma_Curve.Generate'Access));
   --  SHT.Set (new String '("GtkGentry"),
   --    (Gtk.GEntry.Generate'Access, Gtk.GEntry.Generate'Access));
   --  SHT.Set (new String '("GtkHandleBox"),
   --    (Gtk.Handle_Box.Generate'Access, Gtk.Handle_Box.Generate'Access));
   --  SHT.Set (new String '("GtkHButtonBox"),
   --    (Gtk.Hbutton_Box.Generate'Access, Gtk.Hbutton_Box.Generate'Access));
   --  SHT.Set (new String '("GtkImage"),
   --    (Gtk.Image.Generate'Access, Gtk.Image.Generate'Access));
   SHT.Set (new String '("GtkInputDialog"),
     (Gtk.Input_Dialog.Generate'Access, Gtk.Input_Dialog.Generate'Access));
   SHT.Set (new String '("GtkItem"),
     (Gtk.Item.Generate'Access, Gtk.Item.Generate'Access));
   SHT.Set (new String '("GtkLabel"),
     (Gtk.Label.Generate'Access, Gtk.Label.Generate'Access));
   --  SHT.Set (new String '("GtkList"),
   --    (Gtk.List.Generate'Access, Gtk.List.Generate'Access));
   SHT.Set (new String '("GtkListItem"),
     (Gtk.List_Item.Generate'Access, Gtk.List_Item.Generate'Access));
   SHT.Set (new String '("GtkMenu"),
     (Gtk.Menu.Generate'Access, Gtk.Menu.Generate'Access));
   SHT.Set (new String '("GtkMenuBar"),
     (Gtk.Menu_Bar.Generate'Access, Gtk.Menu_Bar.Generate'Access));
   SHT.Set (new String '("GtkMenuItem"),
     (Gtk.Menu_Item.Generate'Access, Gtk.Menu_Item.Generate'Access));
   SHT.Set (new String '("GtkMenuShell"),
     (Gtk.Menu_Shell.Generate'Access, Gtk.Menu_Shell.Generate'Access));
   SHT.Set (new String '("GtkNotebook"),
     (Gtk.Notebook.Generate'Access, Gtk.Notebook.Generate'Access));
   SHT.Set (new String '("GtkOptionMenu"),
     (Gtk.Option_Menu.Generate'Access, Gtk.Option_Menu.Generate'Access));
   --  SHT.Set (new String '("GtkPaned"),
   --    (Gtk.Paned.Generate'Access, Gtk.Paned.Generate'Access));
   --  SHT.Set (new String '("GtkPixmap"),
   --    (Gtk.Pixmap.Generate'Access, Gtk.Pixmap.Generate'Access));
   SHT.Set (new String '("GtkPreview"),
     (Gtk.Preview.Generate'Access, Gtk.Preview.Generate'Access));
   SHT.Set (new String '("GtkProgressBar"),
     (Gtk.Progress_Bar.Generate'Access, Gtk.Progress_Bar.Generate'Access));
   SHT.Set (new String '("GtkRadioButton"),
     (Gtk.Radio_Button.Generate'Access, Gtk.Radio_Button.Generate'Access));
   --  SHT.Set (new String '("GtkRadioMenuItem"),
   --    (Gtk.Radio_Menu_Item.Generate'Access,
   --     Gtk.Radio_Menu_Item.Generate'Access));
   SHT.Set (new String '("GtkRuler"),
     (Gtk.Ruler.Generate'Access, Gtk.Ruler.Generate'Access));
   SHT.Set (new String '("GtkHRuler"),
     (Gtk.Ruler.Generate'Access, Gtk.Ruler.Generate'Access));
   SHT.Set (new String '("GtkVRuler"),
     (Gtk.Ruler.Generate'Access, Gtk.Ruler.Generate'Access));
   SHT.Set (new String '("GtkHScale"),
     (Gtk.Scale.Generate'Access, Gtk.Scale.Generate'Access));
   SHT.Set (new String '("GtkVScale"),
     (Gtk.Scale.Generate'Access, Gtk.Scale.Generate'Access));
   SHT.Set (new String '("GtkHScrollbar"),
     (Gtk.Scrollbar.Generate'Access, Gtk.Scrollbar.Generate'Access));
   SHT.Set (new String '("GtkVScrollbar"),
     (Gtk.Scrollbar.Generate'Access, Gtk.Scrollbar.Generate'Access));
   SHT.Set (new String '("GtkScrolledWindow"),
     (Gtk.Scrolled_Window.Generate'Access,
      Gtk.Scrolled_Window.Generate'Access));
   SHT.Set (new String '("GtkHSeparator"),
     (Gtk.Separator.Generate'Access, Gtk.Separator.Generate'Access));
   SHT.Set (new String '("GtkVSeparator"),
     (Gtk.Separator.Generate'Access, Gtk.Separator.Generate'Access));
   SHT.Set (new String '("GtkSpinButton"),
     (Gtk.Spin_Button.Generate'Access, Gtk.Spin_Button.Generate'Access));
   SHT.Set (new String '("GtkStatusbar"),
     (Gtk.Status_Bar.Generate'Access, Gtk.Status_Bar.Generate'Access));
   SHT.Set (new String '("GtkTable"),
     (Gtk.Table.Generate'Access, Gtk.Table.Generate'Access));
   SHT.Set (new String '("GtkText"),
     (Gtk.Text.Generate'Access, Gtk.Text.Generate'Access));
   --  SHT.Set (new String '("GtkTipsQuery"),
   --    (Gtk.Tips_Query.Generate'Access, Gtk.Tips_Query.Generate'Access));
   SHT.Set (new String '("GtkToggleButton"),
     (Gtk.Toggle_Button.Generate'Access,
      Gtk.Toggle_Button.Generate'Access));
   SHT.Set (new String '("GtkToolbar"),
     (Gtk.Toolbar.Generate'Access, Gtk.Toolbar.Generate'Access));
   --  SHT.Set (new String '("GtkTooltips"),
   --    (Gtk.Tooltips.Generate'Access, Gtk.Tooltips.Generate'Access));
   SHT.Set (new String '("GtkTree"),
     (Gtk.Tree.Generate'Access, Gtk.Tree.Generate'Access));
   SHT.Set (new String '("GtkTreeItem"),
     (Gtk.Tree_Item.Generate'Access, Gtk.Tree_Item.Generate'Access));
   --  SHT.Set (new String '("GtkVButtonBox"),
   --    (Gtk.VButton_Box.Generate'Access, Gtk.VButton_Box.Generate'Access));
   --  SHT.Set (new String '("GtkViewport"),
   --    (Gtk.Viewport.Generate'Access, Gtk.Viewport.Generate'Access));
   SHT.Set (new String '("GtkWindow"),
     (Gtk.Window.Generate'Access, Gtk.Window.Generate'Access));
end Gtk.Glade;

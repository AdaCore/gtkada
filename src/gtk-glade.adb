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

with Gtk.Adjustment;
with Gtk.Alignment;
with Gtk.Arrow;
with Gtk.Aspect_Frame;
with Gtk.Bin;
with Gtk.Box;
with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Calendar;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;
with Gtk.CList;
with Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Curve;
with Gtk.Data;
with Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Editable;
with Gtk.Event_Box;
with Gtk.File_Selection;
with Gtk.Fixed;
with Gtk.Font_Selection;
with Gtk.Frame;
with Gtk.Gamma_Curve;
with Gtk.GEntry;
with Gtk.GRange;
with Gtk.Handle_Box;
with Gtk.HButton_Box;
with Gtk.Image;
with Gtk.Input_Dialog;
with Gtk.Item;
with Gtk.Label;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Misc;
with Gtk.Notebook;
with Gtk.Option_Menu;
with Gtk.Paned;
with Gtk.Pixmap;
with Gtk.Preview;
with Gtk.Progress;
with Gtk.Progress_Bar;
with Gtk.Radio_Button;
with Gtk.Radio_Menu_Item;
with Gtk.Ruler;
with Gtk.Scale;
with Gtk.Scrollbar;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Spin_Button;
with Gtk.Status_Bar;
with Gtk.Table;
with Gtk.Text;
with Gtk.Tips_Query;
with Gtk.Toggle_Button;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Tree;
with Gtk.Tree_Item;
with Gtk.VButton_Box;
with Gtk.Viewport;
with Gtk.Widget;
with Gtk.Window;

with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;
with GNAT.HTable;

package body Gtk.Glade is

   type Gtk_Object_Ptr is access all Gtk.Object.Gtk_Object'Class;

   Max_Widgets : constant := 100;
   --  Maximum number of widgets

   subtype Hash_Header is Natural range 0 .. 150;
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
      Element    => Gtk_Object_Ptr,
      No_Element => new Gtk.Object.Gtk_Object,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   procedure Create_Widget (N : Node_Ptr);
   --  Create a widget and all its children from a given widget tree

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

   procedure Create_Widget (N : Node_Ptr) is
      P   : Node_Ptr;
      Obj : Gtk.Object.Gtk_Object'Class := Get_Object (Get_Field (N, "class"));

   begin
      Gtk.Object.Generate (Obj, N);
      Widget.Show (Gtk.Widget.Gtk_Widget (Obj));
      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            Create_Widget (P);
         end if;

         P := P.Next;
      end loop;
   end Create_Widget;

   ---------------------------
   -- Print_Create_Function --
   ---------------------------

   procedure Print_Create_Function
     (N : Node_Ptr; File : File_Type; First : Boolean := False)
   is
      P : Node_Ptr;
      S : String_Ptr;

   begin
      Gtk.Object.Generate (Get_Object (Get_Field (N, "class")), N, File);
      S := Get_Field (N, "name");

      if S /= null then
         if not First then
            Put_Line (File, "   Widget.Show (Gtk_Widget (" &
              To_Ada (S.all) & "));");
         end if;

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
         Put_Line ("with Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) &
           "; use Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) & ";");
         Put_Line ("with Create_" & To_Ada (P.all) & ";");
         M := M.Next;
      end loop;

      New_Line;
      Put_Line ("procedure " & To_Ada (R.all) & " is");

      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");
         Q := Get_Field (M, "class");
         Put_Line ("   " & To_Ada (P.all) & " : " & To_Ada (Q.all) & ";");
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
         Put_Line ("   " & To_Ada (P.all) & " := Create_" &
           To_Ada (P.all) & ";");
         Put_Line ("   Widget.Show (Gtk_Widget (" & To_Ada (P.all) & "));");
         M := M.Next;
      end loop;

      Put_Line ("   Gtk.Main.Main;");
      Put_Line ("end " & To_Ada (R.all) & ";");

      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");
         Q := Get_Field (M, "class");
         New_Line;
         Put_Line ("with Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) &
           "; use Gtk." & To_Ada (Q (Q'First + 3 .. Q'Last)) & ";");
         New_Line;
         Put_Line ("function Create_" & To_Ada (P.all) & " return " &
           To_Ada (Q.all) & ";");
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

   begin
      while P /= null loop
         if P.Tag.all = "widget" then
            Q := Find_Tag (P.Child, "name");

            if Q /= null then
               Put_Line (File, "   " & To_Ada (Q.Value.all) & " : " &
                 To_Ada (Get_Field (P, "class").all) & ";");
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
      N, M        : Node_Ptr;
      Buffer      : String (1 .. 256);
      Len         : Natural;
      Num_Signals : Natural;
      Output      : File_Type;

   begin
      N := Parse (File);
      Print_Header (N, Output);
      M := N.Child.Next;

      loop
         exit when M = null;

         Create (Output);
         Put_Line (Output, "function Create_" &
           To_Ada (Get_Field (M, "name").all) & " return " &
           To_Ada (Get_Field (M, "class").all) & " is");
         Put_Line (Output, "   Cb_Id : Glib.Guint;");
         Print_Var (M, Output);
         New_Line (Output);
         Put_Line (Output, "begin");
         Print_Create_Function (M, Output, True);
         Print_Foot_Page (M, Output);
         New_Line;
         Put_Line ("with Glib;");
         Put_Line ("with Gtk; use Gtk;");
         Put_Line ("with Gtk.Enums; use Gtk.Enums;");
         Put_Line ("with Gtk.Button; use Gtk.Button;");
         Put_Line ("with Callbacks; use Callbacks;");
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

         Close (Output);
         M := M.Next;
      end loop;

      New_Line;
      Num_Signals := Gen_Signal_Instanciations (Standard_Output);
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
         Create_Widget (P);
         P := P.Next;
      end loop;
   end Instanciate;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Class : String_Ptr)
     return Gtk.Object.Gtk_Object'Class is
   begin
      return SHT.Get (Class).all;
   end Get_Object;

   Object : Gtk_Object_Ptr;

begin
   SHT.Set (new String '("GtkAdjustement"), new Gtk.Adjustment.Gtk_Adjustment);
   SHT.Set (new String '("GtkAlignment"), new Gtk.Alignment.Gtk_Alignment);
   SHT.Set (new String '("GtkArrow"), new Gtk.Arrow.Gtk_Arrow);
   SHT.Set (new String '("GtkAspectFrame"),
            new Gtk.Aspect_Frame.Gtk_Aspect_Frame);
   SHT.Set (new String '("GtkBin"), new Gtk.Bin.Gtk_Bin);

   Object := new Gtk.Box.Gtk_Box;
   SHT.Set (new String '("GtkBox"), Object);
   SHT.Set (new String '("GtkHBox"), Object);
   SHT.Set (new String '("GtkVBox"), Object);

   SHT.Set (new String '("GtkButton"), new Gtk.Button.Gtk_Button);
   SHT.Set (new String '("GtkButtonBox"), new Gtk.Button_Box.Gtk_Button_Box);
   SHT.Set (new String '("GtkCalendar"), new Gtk.Calendar.Gtk_Calendar);
   SHT.Set (new String '("GtkCheckButton"),
            new Gtk.Check_Button.Gtk_Check_Button);
   SHT.Set (new String '("GtkCheckMenuItem"),
            new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item);
   SHT.Set (new String '("GtkCList"), new Gtk.CList.Gtk_CList);
   SHT.Set (new String '("GtkColorSelection"),
            new Gtk.Color_Selection.Gtk_Color_Selection);
   SHT.Set (new String '("GtkColorSelectionDialog"),
            new Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog);
   SHT.Set (new String '("GtkCombo"), new Gtk.Combo.Gtk_Combo);
   SHT.Set (new String '("GtkContainer"), new Gtk.Container.Gtk_Container);
   SHT.Set (new String '("GtkCurve"), new Gtk.Curve.Gtk_Curve);
   SHT.Set (new String '("GtkData"), new Gtk.Data.Gtk_Data);
   SHT.Set (new String '("GtkDialog"), new Gtk.Dialog.Gtk_Dialog);
   SHT.Set (new String '("GtkDrawingArea"),
            new Gtk.Drawing_Area.Gtk_Drawing_Area);
   SHT.Set (new String '("GtkEditable"), new Gtk.Editable.Gtk_Editable);
   SHT.Set (new String '("GtkEventBox"), new Gtk.Event_Box.Gtk_Event_Box);
   SHT.Set (new String '("GtkFileSelection"),
            new Gtk.File_Selection.Gtk_File_Selection);
   SHT.Set (new String '("GtkFixed"), new Gtk.Fixed.Gtk_Fixed);
   SHT.Set (new String '("GtkFontSelection"),
            new Gtk.Font_Selection.Gtk_Font_Selection);
   SHT.Set (new String '("GtkFrame"), new Gtk.Frame.Gtk_Frame);
   SHT.Set (new String '("GtkGammaCurve"),
            new Gtk.Gamma_Curve.Gtk_Gamma_Curve);
   SHT.Set (new String '("GtkGentry"), new Gtk.GEntry.Gtk_Entry);
   SHT.Set (new String '("GtkGrange"), new Gtk.GRange.Gtk_Range);
   SHT.Set (new String '("GtkHandleBox"), new Gtk.Handle_Box.Gtk_Handle_Box);
   SHT.Set (new String '("GtkHButtonBox"),
     new Gtk.HButton_Box.Gtk_HButton_Box);
   SHT.Set (new String '("GtkImage"), new Gtk.Image.Gtk_Image);
   SHT.Set (new String '("GtkImage"), new Gtk.Image.Gtk_Image);
   SHT.Set (new String '("GtkInputDialog"),
            new Gtk.Input_Dialog.Gtk_Input_Dialog);
   SHT.Set (new String '("GtkItem"), new Gtk.Item.Gtk_Item);
   SHT.Set (new String '("GtkLabel"), new Gtk.Label.Gtk_Label);
   SHT.Set (new String '("GtkList"), new Gtk.List.Gtk_List);
   SHT.Set (new String '("GtkListItem"), new Gtk.List_Item.Gtk_List_Item);
   SHT.Set (new String '("GtkMenu"), new Gtk.Menu.Gtk_Menu);
   SHT.Set (new String '("GtkMenuBar"), new Gtk.Menu_Bar.Gtk_Menu_Bar);
   SHT.Set (new String '("GtkMenuItem"), new Gtk.Menu_Item.Gtk_Menu_Item);
   SHT.Set (new String '("GtkMenuShell"), new Gtk.Menu_Shell.Gtk_Menu_Shell);
   SHT.Set (new String '("GtkMisc"), new Gtk.Misc.Gtk_Misc);
   SHT.Set (new String '("GtkNotebook"), new Gtk.Notebook.Gtk_Notebook);
   SHT.Set (new String '("GtkOptionMenu"),
            new Gtk.Option_Menu.Gtk_Option_Menu);
   SHT.Set (new String '("GtkPaned"), new Gtk.Paned.Gtk_Paned);
   SHT.Set (new String '("GtkPixmap"), new Gtk.Pixmap.Gtk_Pixmap);
   SHT.Set (new String '("GtkPreview"), new Gtk.Preview.Gtk_Preview);
   SHT.Set (new String '("GtkProgress"), new Gtk.Progress.Gtk_Progress);
   SHT.Set (new String '("GtkProgressBar"),
            new Gtk.Progress_Bar.Gtk_Progress_Bar);
   SHT.Set (new String '("GtkRadioButton"),
            new Gtk.Radio_Button.Gtk_Radio_Button);
   SHT.Set (new String '("GtkRadioMenuItem"),
            new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item);

   Object := new Gtk.Ruler.Gtk_Ruler;
   SHT.Set (new String '("GtkRuler"), Object);
   SHT.Set (new String '("GtkHRuler"), Object);
   SHT.Set (new String '("GtkVRuler"), Object);

   SHT.Set (new String '("GtkScale"), new Gtk.Scale.Gtk_Scale);
   SHT.Set (new String '("GtkScrollbar"), new Gtk.Scrollbar.Gtk_Scrollbar);
   SHT.Set (new String '("GtkScrolledWindow"),
            new Gtk.Scrolled_Window.Gtk_Scrolled_Window);
   SHT.Set (new String '("GtkSeparator"), new Gtk.Separator.Gtk_Separator);
   SHT.Set (new String '("GtkSpinButton"),
            new Gtk.Spin_Button.Gtk_Spin_Button);
   SHT.Set (new String '("GtkStatusBar"), new Gtk.Status_Bar.Gtk_Status_Bar);
   SHT.Set (new String '("GtkTable"), new Gtk.Table.Gtk_Table);
   SHT.Set (new String '("GtkText"), new Gtk.Text.Gtk_Text);
   SHT.Set (new String '("GtkTipsQuery"), new Gtk.Tips_Query.Gtk_Tips_Query);
   SHT.Set (new String '("GtkToggleButton"),
            new Gtk.Toggle_Button.Gtk_Toggle_Button);
   SHT.Set (new String '("GtkToolbar"), new Gtk.Toolbar.Gtk_Toolbar);
   SHT.Set (new String '("GtkTooltips"), new Gtk.Tooltips.Gtk_Tooltips);
   SHT.Set (new String '("GtkTree"), new Gtk.Tree.Gtk_Tree);
   SHT.Set (new String '("GtkTreeItem"), new Gtk.Tree_Item.Gtk_Tree_Item);
   SHT.Set (new String '("GtkVButtonBox"),
            new Gtk.VButton_Box.Gtk_VButton_Box);
   SHT.Set (new String '("GtkViewport"), new Gtk.Viewport.Gtk_Viewport);
   SHT.Set (new String '("GtkWidget"), new Gtk.Widget.Gtk_Widget);
   SHT.Set (new String '("GtkWindow"), new Gtk.Window.Gtk_Window);
end Gtk.Glade;

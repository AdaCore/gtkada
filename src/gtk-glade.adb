-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--   Copyright (C) 1999-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
--                                                                   --
-- GATE is free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.HTable;
with Gtk_Generates; use Gtk_Generates;

package body Gtk.Glade is

   Max_Widgets : constant := 200;
   --  Maximum number of widget types.

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
      Element    => Generate_Ptr,
      No_Element => null,
      Key        => String_Ptr,
      Hash       => Hash,
      Equal      => Equal);

   procedure Print_Initialize_Procedure
     (N : Node_Ptr; File : File_Type);
   --  Print body of a given "Initialize" procedure to file.

   procedure Print_Header (N : Node_Ptr; File : File_Type);
   --  Print the main procedure with the name of the project contained in N
   --  to file.

   type Variable_Kind is (Global, Local);

   function Print_Var
     (N           : Node_Ptr;
      File        : File_Type;
      Kind        : Variable_Kind) return Boolean;
   --  Print variable declarations for a given "create" function to file.
   --  If Kind is Global, print only the field as described in the node N,
   --  otherwise print only the variables used internally to support the
   --  global variables (e.g temporary lists, ...)
   --  Return True if at least one variable has been printed.

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

   --------------------------------
   -- Print_Initialize_Procedure --
   --------------------------------

   procedure Print_Initialize_Procedure (N : Node_Ptr; File : File_Type) is
      P : Node_Ptr;
      S : String_Ptr;
      C : Boolean;

   begin
      C := N.Specific_Data.Created;

      S := Get_Field (N, "class");
      Get_Gate (S.all) (N, File);
      End_Generate (N, File);

      if not C and then S.all /= "Placeholder" then
         New_Line (File);
      end if;

      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            Print_Initialize_Procedure (P, File);
         end if;

         P := P.Next;
      end loop;
   end Print_Initialize_Procedure;

   ------------------
   -- Print_Header --
   ------------------

   procedure Print_Header (N : Node_Ptr; File : File_Type) is
      P, Q : String_Ptr;
      M : Node_Ptr;

   begin
      P := Get_Field (N.Child.Next, "name");
      Q := Get_Field (N.Child, "name");

      Put_Line (File, "with Gtk; use Gtk;");
      Put_Line (File, "with Gtk.Main;");
      Put_Line (File, "with Gtk.Widget; use Gtk.Widget;");

      M := N.Child.Next;

      loop
         exit when M = null;

         P := Get_Field (M, "name");

         if P /= null then
            Put_Line (File, "with " & To_Ada (P.all) & "_Pkg; use " &
              To_Ada (P.all) & "_Pkg;");
         end if;

         M := M.Next;
      end loop;

      New_Line (File);
      Put_Line (File, "procedure " & To_Ada (Q.all) & " is");

      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");

         if P /= null then
            Put_Line (File,
              "   " & To_Ada (P.all) & " : " & To_Ada (P.all) & "_Access;");
         end if;

         M := M.Next;
      end loop;

      New_Line (File);
      Put_Line (File, "begin");
      Put_Line (File, "   Gtk.Main.Set_Locale;");
      Put_Line (File, "   Gtk.Main.Init;");
      M := N.Child.Next;

      loop
         exit when M = null;
         P := Get_Field (M, "name");

         if P /= null then
            Put_Line (File, "   Gtk_New (" & To_Ada (P.all) & ");");
            Put_Line (File, "   Show_All (" & To_Ada (P.all) & ");");
         end if;

         M := M.Next;
      end loop;

      Put_Line (File, "   Gtk.Main.Main;");
      Put_Line (File, "end " & To_Ada (Q.all) & ";");
   end Print_Header;

   ---------------
   -- Print_Var --
   ---------------

   function Print_Var
     (N           : Node_Ptr;
      File        : File_Type;
      Kind        : Variable_Kind) return Boolean
  is
      Option_Menu : Boolean := True;
      Accelerator : Boolean := True;
      Tooltip     : Boolean := True;
      Image       : Boolean := True;
      Printed     : Boolean := False;

      procedure Print_Var
        (N     : Node_Ptr;
         File  : File_Type;
         Kind  : Variable_Kind;
         First : Boolean;
         Option_Menu : in out Boolean;
         Accelerator : in out Boolean;
         Tooltip     : in out Boolean);
      --  Internal recursive version of this procedure

      procedure Print_Var
        (N     : Node_Ptr;
         File  : File_Type;
         Kind  : Variable_Kind;
         First : Boolean;
         Option_Menu : in out Boolean;
         Accelerator : in out Boolean;
         Tooltip     : in out Boolean)
      is
         P : Node_Ptr := N;
         Q : Node_Ptr;
         R : String_Ptr;
         S : String_Ptr;

      begin
         while P /= null loop
            if P.Tag.all = "widget" then
               Q := Find_Tag (P.Child, "name");

               if Q /= null then
                  S := Get_Field (P, "class");

                  if Kind = Global then
                     if not First then
                        Put (File, "      " & To_Ada (Q.Value.all) & " : ");
                        R := Get_Field (P, "child_name");

                        if R /= null
                          and then Get_Part (R.all, 1) = "Toolbar"
                        then
                           Put_Line (File, "Gtk_Widget;");
                        else
                           Put_Line (File, To_Ada (S.all) & ";");
                        end if;

                        Printed := True;
                     end if;

                  elsif Kind = Local then
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
                        Printed := True;
                     end if;

                     --  Declare a GSList for each Widget containing radio
                     --  buttons or radio menu items

                     if S.all = "GtkRadioButton"
                      or else S.all = "GtkRadioMenuItem"
                     then
                        if not
                          P.Parent.Specific_Data.Has_Radio_Group
                        then
                           Put_Line (File, "   " &
                             To_Ada (Get_Field (P.Parent, "name").all) &
                             "_Group : Widget_SList.GSList;");
                           P.Parent.Specific_Data.Has_Radio_Group := True;
                           Printed := True;
                        end if;
                     end if;

                     --  Declare a Glist with each combo box

                     if S.all = "GtkCombo" then
                        Put_Line (File, "   " &
                          To_Ada (Get_Field (P, "name").all) &
                          "_Items : String_List.Glist;");
                        Printed := True;
                     end if;

                     --  Declare a menu with each option menu

                     if S.all = "GtkOptionMenu" then
                        Put_Line (File, "   " &
                          To_Ada (Get_Field (P, "name").all) &
                          "_Menu : Gtk_Menu;");
                        Printed := True;

                        if Option_Menu then
                           Put_Line (File,
                             "   The_Menu_Item : Gtk_Menu_Item;");
                           Option_Menu := False;
                        end if;
                     end if;

                     --  Declare a Gdk_Image and a Gdk_Visual if any Gtk_Image
                     --  needs to be created in this widget

                     if S.all = "GtkImage" then
                        if Image then
                           Put_Line (File, "   The_Image  : Gdk_Image;");
                           Put_Line (File, "   The_Visual : Gdk_Visual;");
                           Image := False;
                           Printed := True;
                        end if;
                     end if;

                     --  Declare an Accel_Group if any accelerator needs to be
                     --  set up in this widget

                     if Find_Tag (P.Child, "accelerator") /= null then
                        if Accelerator then
                           Put_Line (File,
                             "   The_Accel_Group : Gtk_Accel_Group;");
                           Accelerator := False;
                           Printed := True;
                        end if;
                     end if;

                     --  Declare a Tooltip if any tooltip needs to be
                     --  set up in this widget

                     if Find_Tag (P.Child, "tooltip") /= null then
                        if Tooltip then
                           Put_Line (File,
                             "   Tooltips : Gtk_Tooltips;");
                           Tooltip := False;
                           Printed := True;
                        end if;
                     end if;
                  end if;

                  Print_Var (P.Child, File, Kind, False, Option_Menu,
                    Accelerator, Tooltip);
               end if;
            end if;

            exit when First;

            P := P.Next;
         end loop;
      end Print_Var;

   begin
      Print_Var (N, File, Kind, True, Option_Menu, Accelerator, Tooltip);
      return Printed;
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
      Gettext     : Boolean;

   begin
      Print_Header (N, Standard_Output);
      M := N.Child.Next;
      Gettext := Gettext_Support (M);

      loop
         exit when M = null;

         Name  := Get_Field (M, "name");
         Class := Get_Field (M, "class");

         if Name /= null and Class /= null then
            Create (Output);
            Put_Line (Output, "package " & To_Ada (Name.all) & "_Pkg is");
            New_Line (Output);
            Put_Line (Output, "   type " & To_Ada (Name.all) &
              "_Record is new " & To_Ada (Class.all) & "_Record with record");

            if not Print_Var (M, Output, Global) then
               Put_Line (Output, "      null;");
            end if;

            Put_Line (Output, "   end record;");
            Put_Line (Output, "   type " & To_Ada (Name.all) &
              "_Access is access all " & To_Ada (Name.all) & "_Record'Class;");
            New_Line (Output);
            Put_Line (Output, "   procedure Gtk_New (" & To_Ada (Name.all) &
              " : out " & To_Ada (Name.all) & "_Access);");
            Put_Line (Output, "   procedure Initialize (" & To_Ada (Name.all) &
              " : access " & To_Ada (Name.all) & "_Record'Class);");
            New_Line (Output);
            Put_Line (Output, "end " & To_Ada (Name.all) & "_Pkg;");

            Put_Line (Output, "with Glib; use Glib;");
            Put_Line (Output, "with Gtk; use Gtk;");

            --  ??? It would be nice to determine when these packages are
            --  needed

            Put_Line (Output, "with Gdk.Types;       use Gdk.Types;");
            Put_Line (Output, "with Gtk.Widget;      use Gtk.Widget;");
            Put_Line (Output, "with Gtk.Enums;       use Gtk.Enums;");
            Put_Line (Output, "with Gtkada.Handlers; use Gtkada.Handlers;");
            Put_Line (Output, "with Callbacks_" & Project &
              "; use Callbacks_" & Project & ";");

            if Gettext then
               Put_Line
                 (Output, "with " & Project & "_Intl; use " & Project &
                  "_Intl;");
            end if;

            if Find_Child (M.Child, "handler") /= null then
               Put_Line (Output, "with " & To_Ada (Name.all) &
                 "_Pkg.Callbacks; use " & To_Ada (Name.all) &
                 "_Pkg.Callbacks;");
            end if;

            New_Line (Output);
            Put_Line (Output, "package body " & To_Ada (Name.all) &
              "_Pkg is");
            New_Line (Output);
            Put_Line (Output, "procedure Gtk_New (" &
              To_Ada (Name.all) & " : out " & To_Ada (Name.all) &
              "_Access) is");
            Put_Line (Output, "begin");
            Put_Line (Output, "   " & To_Ada (Name.all) & " := new " &
              To_Ada (Name.all) & "_Record;");
            Put_Line (Output, "   " & To_Ada (Name.all) & "_Pkg.Initialize (" &
              To_Ada (Name.all) & ");");
            Put_Line (Output, "end Gtk_New;");
            New_Line (Output);
            Put_Line (Output, "procedure Initialize (" &
              To_Ada (Name.all) & " : access " & To_Ada (Name.all) &
              "_Record'Class) is");
            Put_Line (Output, "   pragma Suppress (All_Checks);");

            if Print_Var (M, Output, Local) then
               New_Line (Output);
            end if;

            Put_Line (Output, "begin");
            Print_Initialize_Procedure (M, Output);
            Put_Line (Output, "end Initialize;");
            New_Line (Output);
            Put_Line (Output, "end " & To_Ada (Name.all) & "_Pkg;");

            --  Add "predefined" packages

            Add_Package ("Button");

            Gen_Packages (Standard_Output);
            Reset_Packages;
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

      if Gettext then
         Put_Line ("package " & Project & "_Intl is");
         New_Line;
         Put_Line ("   function ""-"" (Msg : String) return String;");
         Put_Line ("   --  Convenient shortcut to the Gettext function.");
         New_Line;
         Put_Line ("end " & Project & "_Intl;");
         Put_Line ("with Gtkada.Intl; use Gtkada.Intl;");
         New_Line;
         Put_Line ("package body " & Project & "_Intl is");
         New_Line;
         Put_Line ("   function ""-"" (Msg : String) return String is");
         Put_Line ("   begin");
         Put_Line ("      return Dgettext (""" & Project & """, Msg);");
         Put_Line ("   end ""-"";");
         New_Line;
         Put_Line ("end " & Project & "_Intl;");
      end if;

      Num_Signals := Gen_Signal_Instantiations (Project, Standard_Output);
   end Generate;

   --------------
   -- Get_Gate --
   --------------

   function Get_Gate (Class : String) return Generate_Ptr is
      S   : aliased String := Class;
      Ptr : Generate_Ptr;
   begin
      Ptr := SHT.Get (S'Unchecked_Access);

      if Ptr = null then
         return Generic_Ptr'Access;
      else
         return Ptr;
      end if;
   end Get_Gate;

   -----------------------
   -- Register_Generate --
   -----------------------

   procedure Register_Generate (Widget : String; Generate : Generate_Ptr) is
   begin
      SHT.Set (new String'(Widget), Generate);
   end Register_Generate;

begin
   SHT.Set (new String'("GtkAccelLabel"), Accel_Label_Generate'Access);
   SHT.Set (new String'("GtkAlignment"), Alignment_Generate'Access);
   SHT.Set (new String'("GtkArrow"), Arrow_Generate'Access);
   SHT.Set (new String'("GtkAspectFrame"), Aspect_Frame_Generate'Access);
   SHT.Set (new String'("GtkBox"), Box_Generate'Access);
   SHT.Set (new String'("GtkHBox"), Box_Generate'Access);
   SHT.Set (new String'("GtkVBox"), Box_Generate'Access);
   SHT.Set (new String'("GtkButton"), Button_Generate'Access);
   SHT.Set (new String'("GtkCalendar"), Calendar_Generate'Access);
   SHT.Set (new String'("GtkCheckButton"), Check_Button_Generate'Access);
   SHT.Set (new String'("GtkCheckMenuItem"), Check_Menu_Item_Generate'Access);
   SHT.Set (new String'("GtkCList"), Clist_Generate'Access);
   SHT.Set (new String'("GtkColorSelection"), Color_Selection_Generate'Access);
   SHT.Set (new String'("GtkColorSelectionDialog"),
     Color_Selection_Dialog_Generate'Access);
   SHT.Set (new String'("GtkCombo"), Combo_Generate'Access);
   SHT.Set (new String'("GtkCTree"), Ctree_Generate'Access);
   SHT.Set (new String'("GtkCurve"), Curve_Generate'Access);
   SHT.Set (new String'("GtkDialog"), Dialog_Generate'Access);
   SHT.Set (new String'("GtkDrawingArea"), Drawing_Area_Generate'Access);
   SHT.Set (new String'("GtkEditable"), Editable_Generate'Access);
   SHT.Set (new String'("GtkEventBox"), Event_Box_Generate'Access);
   SHT.Set (new String'("GtkFileSelection"), File_Selection_Generate'Access);
   SHT.Set (new String'("GtkFixed"), Fixed_Generate'Access);
   SHT.Set (new String'("GtkFontSelection"), Font_Selection_Generate'Access);
   SHT.Set (new String'("GtkFontSelectionDialog"),
     Font_Selection_Dialog_Generate'Access);
   SHT.Set (new String'("GtkFrame"), Frame_Generate'Access);
   SHT.Set (new String'("GtkGammaCurve"), Gamma_Curve_Generate'Access);
   SHT.Set (new String'("GtkEntry"), GEntry_Generate'Access);
   SHT.Set (new String'("GtkHandleBox"), Handle_Box_Generate'Access);
   SHT.Set (new String'("GtkHButtonBox"), Hbutton_Box_Generate'Access);
   SHT.Set (new String'("GtkImage"), Image_Generate'Access);
   SHT.Set (new String'("GtkInputDialog"), Input_Dialog_Generate'Access);
   SHT.Set (new String'("GtkItem"), Item_Generate'Access);
   SHT.Set (new String'("GtkLabel"), Label_Generate'Access);
   SHT.Set (new String'("GtkLayout"), Layout_Generate'Access);
   SHT.Set (new String'("GtkList"), List_Generate'Access);
   SHT.Set (new String'("GtkListItem"), List_Item_Generate'Access);
   SHT.Set (new String'("GtkMenu"), Menu_Generate'Access);
   SHT.Set (new String'("GtkMenuBar"), Menu_Bar_Generate'Access);
   SHT.Set (new String'("GtkMenuItem"), Menu_Item_Generate'Access);
   SHT.Set (new String'("GtkMenuShell"), Menu_Shell_Generate'Access);
   SHT.Set (new String'("GtkNotebook"), Notebook_Generate'Access);
   SHT.Set (new String'("GtkOptionMenu"), Option_Menu_Generate'Access);
   SHT.Set (new String'("GtkHPaned"), Paned_Generate'Access);
   SHT.Set (new String'("GtkVPaned"), Paned_Generate'Access);
   SHT.Set (new String'("GtkPixmap"), Pixmap_Generate'Access);
   SHT.Set (new String'("GtkPreview"), Preview_Generate'Access);
   SHT.Set (new String'("GtkProgressBar"), Progress_Bar_Generate'Access);
   SHT.Set (new String'("GtkRadioButton"), Radio_Button_Generate'Access);
   SHT.Set (new String'("GtkRadioMenuItem"), Radio_Menu_Item_Generate'Access);
   SHT.Set (new String'("GtkRuler"), Ruler_Generate'Access);
   SHT.Set (new String'("GtkHRuler"), Ruler_Generate'Access);
   SHT.Set (new String'("GtkVRuler"), Ruler_Generate'Access);
   SHT.Set (new String'("GtkHScale"), Scale_Generate'Access);
   SHT.Set (new String'("GtkVScale"), Scale_Generate'Access);
   SHT.Set (new String'("GtkHScrollbar"), Scrollbar_Generate'Access);
   SHT.Set (new String'("GtkVScrollbar"), Scrollbar_Generate'Access);
   SHT.Set (new String'("GtkScrolledWindow"), Scrolled_Window_Generate'Access);
   SHT.Set (new String'("GtkHSeparator"), Separator_Generate'Access);
   SHT.Set (new String'("GtkVSeparator"), Separator_Generate'Access);
   SHT.Set (new String'("GtkSpinButton"), Spin_Button_Generate'Access);
   SHT.Set (new String'("GtkStatusbar"), Status_Bar_Generate'Access);
   SHT.Set (new String'("GtkTable"), Table_Generate'Access);
   SHT.Set (new String'("GtkText"), Text_Generate'Access);
   SHT.Set (new String'("GtkToggleButton"), Toggle_Button_Generate'Access);
   SHT.Set (new String'("GtkToolbar"), Toolbar_Generate'Access);
   SHT.Set (new String'("GtkTree"), Tree_Generate'Access);
   SHT.Set (new String'("GtkTreeItem"), Tree_Item_Generate'Access);
   SHT.Set (new String'("GtkVButtonBox"), Vbutton_Box_Generate'Access);
   SHT.Set (new String'("GtkViewport"), Viewport_Generate'Access);
   SHT.Set (new String'("GtkWindow"), Window_Generate'Access);
end Gtk.Glade;

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

   procedure OS_Exit (Return_Code : Integer);
   pragma Import (C, OS_Exit, "exit");

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
     (Project : Node_Ptr; N : Node_Ptr; File : File_Type);
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
      S : constant String := Get_Attribute (N, "id");
   begin
      if S /= "" then
         Put_Line (File, "   --  WARNING: Unsupported widget " &
           Get_Attribute (N, "class") & " (" & S & ")");
      end if;
   exception
      when Constraint_Error =>
         null;
   end Generic_Ptr;

   --------------------------------
   -- Print_Initialize_Procedure --
   --------------------------------

   procedure Print_Initialize_Procedure
     (Project : Node_Ptr; N : Node_Ptr; File : File_Type)
   is
      P, Q : Node_Ptr;
      S : constant String := Get_Attribute (N, "class");
      C : Boolean;
      Is_Internal : Boolean := False;

   begin
      --  Do not generate code for internal children, since these are handled
      --  directly by their parents
      Is_Internal := Get_Attribute (N.Parent, "internal-child") /= "";
      if Is_Internal then
         return;
      end if;

      C := N.Specific_Data.Created;
      Get_Gate (S) (N, File);
      End_Generate (Project, N, File);

      if not C and then S /= "placeholder" then
         New_Line (File);
      end if;

      P := N.Child;

      while P /= null loop
         if P.Tag.all = "child" then
            Q := P.Child;

            while Q /= null loop
               if Q.Tag.all = "widget" then
                  Print_Initialize_Procedure (Project, Q, File);
               end if;

               Q := Q.Next;
            end loop;
         end if;

         P := P.Next;
      end loop;
   end Print_Initialize_Procedure;

   ------------------
   -- Print_Header --
   ------------------

   procedure Print_Header (N : Node_Ptr; File : File_Type) is
      M : Node_Ptr;
      Q : constant String := Get_Attribute (N.Child, "id");

   begin

      Put_Line (File, "with Gtk; use Gtk;");
      Put_Line (File, "with Gtk; use Gtk;");
      Put_Line (File, "with Gtk.Main;");
      Put_Line (File, "with Gtk.Widget; use Gtk.Widget;");

      M := N.Child;

      loop
         exit when M = null;
         declare
            P : constant String := Get_Attribute (M, "id");
         begin
            if P /= "" then
               Put_Line (File, "with " & To_Ada (P) & "_Pkg; use " &
                 To_Ada (P) & "_Pkg;");
            end if;
         end;
         M := M.Next;
      end loop;

      New_Line (File);
      Put_Line (File, "procedure " & To_Ada (Q) & " is");

      M := N.Child;

      loop
         exit when M = null;
         declare
            P : constant String := Get_Attribute (M, "id");
         begin
            if P /= "" then
               Put_Line (File,
                 "   " & To_Ada (P) & " : " & To_Ada (P) & "_Access;");
            end if;
         end;
         M := M.Next;
      end loop;

      New_Line (File);
      Put_Line (File, "begin");
      Put_Line (File, "   Gtk.Main.Set_Locale;");
      Put_Line (File, "   Gtk.Main.Init;");

      M := N.Child;

      loop
         exit when M = null;
         declare
            P : constant String := Get_Attribute (M, "id");
         begin

            if P /= "" then
               Put_Line (File, "   Gtk_New (" & To_Ada (P) & ");");
               Put_Line (File, "   Show_All (" & To_Ada (P) & ");");
            end if;
         end;
         M := M.Next;
      end loop;

      Put_Line (File, "   Gtk.Main.Main;");
      Put_Line (File, "end " & To_Ada (Q) & ";");
   end Print_Header;

   ---------------
   -- Print_Var --
   ---------------

   function Print_Var
     (N           : Node_Ptr;
      File        : File_Type;
      Kind        : Variable_Kind) return Boolean
   is
      Accelerator : Boolean := True;
      Tooltip     : Boolean := True;
      Printed     : Boolean := False;

      procedure Print_Var
        (N     : Node_Ptr;
         File  : File_Type;
         Kind  : Variable_Kind;
         First : Boolean;
         Accelerator : in out Boolean;
         Tooltip     : in out Boolean);
      --  Internal recursive version of this procedure

      procedure Print_Var
        (N     : Node_Ptr;
         File  : File_Type;
         Kind  : Variable_Kind;
         First : Boolean;
         Accelerator : in out Boolean;
         Tooltip     : in out Boolean)
      is
         P : Node_Ptr := N;
         T : Node_Ptr := N;

      begin
         while P /= null loop
            if P.Tag.all = "child" then
               if Get_Attribute (P, "internal-child") = "" then
                  T := Find_Tag (P.Child, "widget");
               else
                  --  Do not generate variables for internal children, since
                  --  these are handled directly by their parents
                  T := null;
               end if;
            else
               T := P;
            end if;

            if T /= null and then T.Tag.all = "widget" then
               declare
                  Q : constant String := Get_Attribute (T, "id");
               begin
                  if Q /= "" then
                     declare
                        S : constant String := Get_Attribute (T, "class");
                     begin
                        if Kind = Global then
                           if not First then
                              Put (File, "      " & To_Ada (Q) & " : ");
                              declare
                                 R : constant String :=
                                   Get_Attribute (T, "id");
                              begin
                                 if R /= ""
                                    and then Get_Part (R, 1) = "Toolbar"
                                 then
                                    Put_Line (File, "Gtk_Widget;");
                                 else
                                    Put_Line (File, To_Ada (S) & ";");
                                 end if;
                              end;
                              Printed := True;
                           end if;

                           if S = "GtkImageMenuItem" then
                              declare
                                 Image : constant Node_Ptr :=
                                   Find_Tag_With_Attribute
                                     (T.Child, "child", "internal-child",
                                      "image");
                              begin
                                 if Image /= null then
                                    Put_Line (File, "   " &
                                              To_Ada (Get_Name (Image.Child))
                                              & " : Gtk_Image;");
                                 end if;
                              end;
                           end if;

                        elsif Kind = Local then
                           --  Special cases:
                           --  Declare a Gtk_Adjustment with each
                           --  Gtk_Spin_Button,
                           --  Gtk_Scale and Gtk_Scrollbar

                           if S = "GtkSpinButton"
                              or else S = "GtkHScale"
                              or else S = "GtkVScale"
                              or else S = "GtkHScrollbar"
                              or else S = "GtkVScrollbar"
                           then
                              Put_Line (File, "   " & To_Ada (Q) &
                                 "_Adj : Gtk_Adjustment;");
                              Printed := True;

                           --  Declare a GSList for each Widget containing
                           --  radio buttons or radio menu items

                           elsif S = "GtkRadioButton"
                              or else S = "GtkRadioMenuItem"
                           then
                              if not
                                 P.Parent.Specific_Data.Has_Radio_Group
                              then
                                 Put_Line (File, "   " &
                                    To_Ada (Get_Attribute (P.Parent, "id")) &
                                    "_Group : Widget_SList.GSList;");
                                 P.Parent.Specific_Data.Has_Radio_Group :=
                                    True;
                                 Printed := True;
                              end if;

                           --  Declare a Glist with each combo box

                           elsif S = "GtkCombo" then
                              Put_Line (File, "   " &
                                 To_Ada (Q) &
                                 "_Items : String_List.Glist;");
                              Printed := True;

                           end if;

                           --  Declare an Accel_Group if any accelerator needs
                           --  to be set up in this widget

                           if Find_Tag (T.Child, "accelerator") /= null then
                              if Accelerator then
                                 Put_Line (File,
                                    "   The_Accel_Group : Gtk_Accel_Group;");
                                 Accelerator := False;
                                 Printed := True;
                              end if;
                           end if;

                           --  Declare a Tooltip if any tooltip needs to be
                           --  set up in this widget

                           if Get_Property (T, "tooltip") /= null then
                              if Tooltip then
                                 Put_Line (File,
                                    "   Tooltips : Gtk_Tooltips;");
                                 Tooltip := False;
                                 Printed := True;
                              end if;
                           end if;
                        end if;
                     end;

                     Print_Var (T.Child, File, Kind, False,
                        Accelerator, Tooltip);
                  end if;
               end;

            end if;

            exit when First;

            P := P.Next;

         end loop;
      end Print_Var;

   begin
      Print_Var (N, File, Kind, True, Accelerator, Tooltip);
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
      Generate (Parse (File & "p"), Parse (File));
      --  Adding a "p" to the filename as Glade produces a .glade and a
      --  .gladep file now. The .gladep file will disappear in future
      --  versions of Glade.
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (Project : Node_Ptr; Interface : Node_Ptr) is
      M              : Node_Ptr;
      Tmp            : Node_Ptr;
      Buffer         : String (1 .. 256);
      Len            : Natural;
      Num_Signals    : Natural;
      pragma Unreferenced (Num_Signals);
      Output         : File_Type;
      Project_Name   : constant String :=
       To_Ada (Get_Field (Find_Tag (Project, "glade-project"), "name").all);
         --  I_Name   : constant String :=
         --     To_Ada (Get_Field (Find_Tag (Interface, "glade-interface"),
         --    "child").all);
      Gettext        : Boolean := True;

   begin
      M := Interface;

      if M = null then
         Put_Line ("no code to generate. exiting.");
         OS_Exit (1);
         return;
      end if;

      Print_Header (Interface, Standard_Output);
      Tmp := Find_Tag (Project, "gettext_support");

      Gettext := Tmp = null or else Tmp.Value.all /= "FALSE";

      M := M.Child;

      loop
         exit when M = null;

         declare
            Name  : constant String := Get_Attribute (M, "id");
            Class : constant String := Get_Attribute (M, "class");
         begin
            if Name /= "" and Class /= "" then
               Create (Output);
               Put_Line (Output, "package " & To_Ada (Name) &
                  "_Pkg is");
               New_Line (Output);
               Put_Line (Output, "   type " & To_Ada (Name) &
                  "_Record is new " & To_Ada (Class) & "_Record with record");

               if not Print_Var (M, Output, Global) then
                  Put_Line (Output, "      null;");
               end if;

               Put_Line (Output, "   end record;");
               Put_Line (Output, "   type " & To_Ada (Name) &
                 "_Access is access " & To_Ada (Name) & "_Record'Class;");
               New_Line (Output);
               Put_Line (Output, "   procedure Gtk_New (" &
                  To_Ada (Name) & " : out " & To_Ada (Name) & "_Access);");
               Put_Line (Output, "   procedure Initialize (" &
                  To_Ada (Name) & " : access " & To_Ada (Name) &
                  "_Record'Class);");
               New_Line (Output);
               Put_Line (Output, "end " & To_Ada (Name) & "_Pkg;");

               Put_Line (Output, "with Glib; use Glib;");
               Put_Line (Output, "with Gtk; use Gtk;");

               --  ??? It would be nice to determine when these packages are
               --  needed

               Put_Line (Output,
                  "with Gdk.Types; use Gdk.Types;");
               Put_Line (Output,
                  "with Gtk.Widget; use Gtk.Widget;");
               Put_Line (Output,
                  "with Gtk.Enums; use Gtk.Enums;");
               Put_Line (Output,
                  "with Gtkada.Handlers; use Gtkada.Handlers;");
               Put_Line (Output,
                  "with Callbacks_" & Project_Name & "; use Callbacks_" &
                  Project_Name & ";");

               if Gettext then
                  Put_Line
                    (Output, "with " & Project_Name & "_Intl; use " &
                     Project_Name & "_Intl;");
               end if;

               if Find_Child (M.Child, "signal") /= null then
                  Put_Line (Output, "with " & To_Ada (Name) &
                    "_Pkg.Callbacks; use " & To_Ada (Name) &
                    "_Pkg.Callbacks;");
               end if;

               New_Line (Output);
               Put_Line (Output, "package body " & To_Ada (Name) &
                 "_Pkg is");
               New_Line (Output);
               Put_Line (Output, "procedure Gtk_New (" &
                 To_Ada (Name) & " : out " & To_Ada (Name) &
                 "_Access) is");
               Put_Line (Output, "begin");
               Put_Line (Output, "   " & To_Ada (Name) & " := new " &
                 To_Ada (Name) & "_Record;");
               Put_Line (Output,
                  "   " & To_Ada (Name) & "_Pkg.Initialize (" & To_Ada (Name) &
                  ");");
               Put_Line (Output, "end Gtk_New;");
               New_Line (Output);
               Put_Line (Output, "procedure Initialize (" &
                 To_Ada (Name) & " : access " & To_Ada (Name) &
                 "_Record'Class) is");

               Put_Line (Output, "   pragma Suppress (All_Checks);");

               declare
                  Pixmaps : constant Node_Ptr := Find_Tag
                    (Project, "pixmaps_directory");
               begin
                  if Pixmaps /= null then
                     Put_Line (Output, "   Pixmaps_Dir : constant String := """
                               & Pixmaps.Value.all & "/"";");
                  else
                     Put_Line (Output, "   Pixmaps_Dir : constant String := """
                               & "pixmaps" & "/"";");
                  end if;
               end;

               --  ??? Is this still safe? UTF-8 etc...

               if Print_Var (M, Output, Local) then
                  New_Line (Output);
               end if;

               Put_Line (Output, "begin");
               Print_Initialize_Procedure (Project, M, Output);
               Put_Line (Output, "end Initialize;");
               New_Line (Output);
               Put_Line (Output, "end " & To_Ada (Name) & "_Pkg;");

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
         end;

         M := M.Next;
      end loop;

      if Gettext then
         Put_Line ("package " & Project_Name & "_Intl is");
         New_Line;
         Put_Line ("   function ""-"" (Msg : String) return String;");
         Put_Line ("   --  Convenient shortcut to the Gettext function.");
         New_Line;
         Put_Line ("end " & Project_Name & "_Intl;");
         Put_Line ("with Gtkada.Intl; use Gtkada.Intl;");
         New_Line;
         Put_Line ("package body " & Project_Name & "_Intl is");
         New_Line;
         Put_Line ("   function ""-"" (Msg : String) return String is");
         Put_Line ("   begin");
         Put_Line ("      return Dgettext (""" & Project_Name & """, Msg);");
         Put_Line ("   end ""-"";");
         New_Line;
         Put_Line ("end " & Project_Name & "_Intl;");
      end if;

      Num_Signals := Gen_Signal_Instantiations (Project_Name, Standard_Output);

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
   SHT.Set (new String'("GtkColorSelection"), Color_Selection_Generate'Access);
   SHT.Set (new String'("GtkColorSelectionDialog"),
            Color_Selection_Dialog_Generate'Access);
   SHT.Set (new String'("GtkCombo"), Combo_Generate'Access);
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
   SHT.Set (new String'("GtkImageMenuItem"), Image_Menu_Item_Generate'Access);
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
   SHT.Set (new String'("GtkSeparatorMenuItem"),
            Separator_Menu_Item_Generate'Access);
   SHT.Set (new String'("GtkSpinButton"), Spin_Button_Generate'Access);
   SHT.Set (new String'("GtkStatusbar"), Status_Bar_Generate'Access);
   SHT.Set (new String'("GtkTable"), Table_Generate'Access);
   SHT.Set (new String'("GtkTextView"), Text_View_Generate'Access);
   SHT.Set (new String'("GtkToggleButton"), Toggle_Button_Generate'Access);
   SHT.Set (new String'("GtkToolbar"), Toolbar_Generate'Access);
   SHT.Set (new String'("GtkTreeView"), Tree_View_Generate'Access);
   SHT.Set (new String'("GtkVButtonBox"), Vbutton_Box_Generate'Access);
   SHT.Set (new String'("GtkViewport"), Viewport_Generate'Access);
   SHT.Set (new String'("GtkWindow"), Window_Generate'Access);
end Gtk.Glade;

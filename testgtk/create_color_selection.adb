with Gtk; use Gtk;
with Glib; use Glib;
with Gtk.Widget;
with Gtk.Color_Selection;        use Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog; use Gtk.Color_Selection_Dialog;
with Gtk.Button;
with Gtk.Signal;
with Ada.Text_IO;

package body Create_Color_Selection is

   package Color_Sel_Cb is new Signal.Callback
     (Widget_Type => Button.Gtk_Button,
      Data_Type   => Gtk_Color_Selection_Dialog);
   package Color_Changed_Cb is new Signal.Void_Callback
     (Widget_Type => Gtk_Color_Selection);
   package Exit_Cb is new Signal.Object_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget);
   --  Must be instanciated at library level !


   procedure Color_Changed (Dialog : in out Gtk_Color_Selection'Class);
   procedure Color_Ok (Widget : in out Button.Gtk_Button'Class;
                       Dialog : in out Gtk_Color_Selection_Dialog);


   procedure Color_Ok (Widget : in out Button.Gtk_Button'Class;
                       Dialog : in out Gtk_Color_Selection_Dialog)
   is
      Color : Color_Array;
   begin
      Get_Color (Get_Colorsel (Dialog), Color);
      Ada.Text_IO.Put_Line ("Button OK in Color_Selection : ");
      for I in Red .. Opacity loop
         Ada.Text_IO.Put_Line (Color_Index'Image (I)
                               & " => "
                               & Gdouble'Image (Color (I)));
      end loop;
      Set_Color (Get_Colorsel (Dialog), Color);
   end Color_Ok;


   procedure Color_Changed (Dialog : in out Gtk_Color_Selection'Class)
   is
      Color : Color_Array;
   begin
      Get_Color (Dialog, Color);
      Ada.Text_IO.Put_Line ("Color changed : ");
      for I in Red .. Opacity loop
         Ada.Text_IO.Put_Line (Color_Index'Image (I)
                               & " => "
                               & Gdouble'Image (Color (I)));
      end loop;
   end Color_Changed;

   Dialog : Gtk_Color_Selection_Dialog;

   procedure Run
     (Widget : in out Button.Gtk_Button'Class)
   is
      Cb_Id  : Guint;
   begin

      if not Is_Created (Dialog) then
         Gtk_New (Dialog, Title => "Color Selection Dialog");
         Set_Opacity (Get_Colorsel (Dialog), True);
         Set_Update_Policy (Get_Colorsel (Dialog), Enums.Update_Continuous);
         Window.Position (Dialog, Enums.Win_Pos_Mouse);

         Cb_Id := Exit_Cb.Connect (Obj => Dialog,
                                   Name => "destroy",
                                   Func => Gtk.Widget.Destroy'Access,
                                   Slot_Object => Dialog);

         Cb_Id := Color_Changed_Cb.Connect (Get_Colorsel (Dialog),
                                            "color_changed",
                                            Color_Changed'Access);
         Cb_Id := Color_Sel_Cb.Connect (Get_OK_Button (Dialog),
                                        "clicked",
                                        Color_Ok'Access,
                                        Dialog);
         Cb_Id := Exit_Cb.Connect (Get_Cancel_Button (Dialog),
                                   "clicked",
                                   Gtk.Widget.Destroy'Access,
                                   Dialog);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Dialog) then
         Gtk.Widget.Show (Dialog);
      else
         Gtk.Widget.Destroy (Dialog);
      end if;
   end Run;

end Create_Color_Selection;


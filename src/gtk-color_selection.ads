with Gtk.Enums;
with Gtk.Window;

package Gtk.Color_Selection is

   type Gtk_Color_Selection is new Gtk.Window.Gtk_Window with private;

   type Color_Index is (Red, Green, Blue, Opacity);
   type Color_Array is array (Color_Index'Range) of Gdouble;

   procedure Get_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : out Color_Array);
   --  mapping: Get_Color gtkcolorsel.h gtk_color_selection_get_color

   procedure Gtk_New (Widget : out Gtk_Color_Selection);
   --  mapping: Gtk_New gtkcolorsel.h gtk_color_selection_new

   procedure Set_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : in Color_Array);
   --  mapping: Set_Color gtkcolorsel.h gtk_color_selection_set_color

   procedure Set_Opacity (Colorsel    : in Gtk_Color_Selection'Class;
                          Use_Opacity : in Boolean);
   --  mapping: Set_Opacity gtkcolorsel.h gtk_color_selection_set_opacity

   procedure Set_Update_Policy (Colorsel : in Gtk_Color_Selection'Class;
                                Policy   : in Enums.Gtk_Update_Type);
   --  mapping: Set_Update_Policy gtkcolorsel.h \
   --  mapping: gtk_color_selection_set_update_policy

private

   type Gtk_Color_Selection is new Gtk.Window.Gtk_Window with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkcolorsel.h gtk_color_selection_get_type

end Gtk.Color_Selection;

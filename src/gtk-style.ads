with Gdk.Color;
with Gdk.Point;
with Gdk.Window;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Style is

   type Gtk_Style is new Root_Type with private;

   type Points_Array is array (Positive range <>) of Gdk.Point.Gdk_Point;


   procedure Gtk_New (Style : out Gtk_Style);
   --  mapping: Gtk_New gtkstyle.h gtk_style_new

   procedure Copy (Source : in Gtk_Style;
                   Destination : out Gtk_Style);
   --  mapping: Copy gtkstyle.h gtk_style_copy

   function Attach (Style  : in Gtk_Style;
                    Window : in Gdk.Window.Gdk_Window) return Gtk_Style;
   --  mapping: Attach gtkstyle.h gtk_style_attach

   procedure Detach (Style : in out Gtk_Style);
   --  mapping: Detach gtkstyle.h gtk_style_detach

   procedure Set_Background (Style      : in out Gtk_Style;
                             Window     : in     Gdk.Window.Gdk_Window;
                             State_Type : in     Enums.Gtk_State_Type);
   --  mapping: Set_Background gtkstyle.h gtk_style_set_background

   function Get_Bg (Style      : in Gtk_Style;
                    State_Type : in Enums.Gtk_State_Type)
                    return          Gdk.Color.Gdk_Color;

   function Get_Black (Style : in Gtk_Style) return Gdk.Color.Gdk_Color;

   function Get_Style (Widget : in Gtk.Widget.Gtk_Widget'Class)
                       return Gtk.Style.Gtk_Style;
   function Get_White (Style : in Gtk_Style) return Gdk.Color.Gdk_Color;


   procedure Draw_Hline (Style      : in Gtk_Style;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         X1, X2     : in Gint;
                         Y          : in Gint);
   --  mapping: Draw_Hline gtkstyle.h gtk_draw_hline

   procedure Draw_Vline (Style      : in Gtk_Style;
                         Window     : in Gdk.Window.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         Y1, Y2     : in Gint;
                         X          : in Gint);
   --  mapping: Draw_Vline gtkstyle.h gtk_draw_vline

   procedure Draw_Shadow (Style       : in Gtk_Style;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
   --  mapping: Draw_Shadow gtkstyle.h gtk_draw_shadow

   procedure Draw_Polygon (Style      : in Gtk_Style;
                           Window     : in Gdk.Window.Gdk_Window;
                           State_Type : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           Points     : in Points_Array;
                           Fill       : in Gint);
   --  mapping: Draw_Polygon gtkstyle.h gtk_draw_polygon

   procedure Draw_Arrow (Style       : in Gtk_Style;
                         Window      : in Gdk.Window.Gdk_Window;
                         State_Type  : in Enums.Gtk_State_Type;
                         Shadow_Type : in Enums.Gtk_Shadow_Type;
                         Arrow_Type  : in Enums.Gtk_Arrow_Type;
                         Fill        : in Gint;
                         X, Y        : in Gint;
                         Width       : in Gint;
                         Height      : in Gint);
   --  mapping: Draw_Arrow gtkstyle.h gtk_draw_arrow

   procedure Draw_Diamond (Style       : in Gtk_Style;
                           Window      : in Gdk.Window.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           X, Y        : in Gint;
                           Width       : in Gint;
                           Height      : in Gint);
   --  mapping: Draw_Diamond gtkstyle.h gtk_draw_diamond

   procedure Draw_Oval (Style       : in Gtk_Style;
                        Window      : in Gdk.Window.Gdk_Window;
                        State_Type  : in Enums.Gtk_State_Type;
                        Shadow_Type : in Enums.Gtk_Shadow_Type;
                        X, Y        : in Gint;
                        Width       : in Gint;
                        Height      : in Gint);
   --  mapping: Draw_Oval gtkstyle.h gtk_draw_oval

   procedure Draw_String (Style       : in Gtk_Style;
                          Window      : in Gdk.Window.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          X, Y        : in Gint;
                          Str         : in String);
   --  mapping: Draw_String gtkstyle.h gtk_draw_string

   procedure Ref (Object : in out Gtk_Style);
   procedure Unref (Object : in out Gtk_Style);

private

   type Gtk_Style is new Root_Type with null record;

   procedure Adjust (Object : in out Gtk_Style);
   procedure Finalize (Object : in out Gtk_Style);
   procedure Initialize (Object : in out Gtk_Style);

   --  mapping: Ref gtkstyle.h gtk_style_ref
   --  mapping: Unref gtkstyle.h gtk_style_unref

end Gtk.Style;

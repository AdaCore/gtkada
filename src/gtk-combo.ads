
with Gtk.Hbox;
with Gtk.Item;
with Glib.Glist;

package Gtk.Combo is

   type String_Access is access all String;
   package String_List is new Glib.Glist.Generic_List (String_Access);

   function To_String_Access (S : String) return String_Access;
   --  Convert a String to a String_Access
   procedure Free (S : in out String_Access);
   --  Free the memory occupied by a String_Access

   type Gtk_Combo is new Gtk.Hbox.Gtk_Hbox with private;

   procedure Disable_Activate (Combo_Box : in Gtk_Combo'Class);
   --  mapping: Disable_Activate gtkcombo.h gtk_combo_disable_activate

   procedure Gtk_New (Widget      : out Gtk_Combo);
   --  mapping: Gtk_New gtkcombo.h gtk_combo_new

   procedure Set_Case_Sensitive (Combo_Box : in Gtk_Combo'Class;
                                 Val       : in Boolean);
   --  mapping: Set_Case_Sensitive gtkcombo.h gtk_combo_set_case_sensitive

   procedure Set_Item_String (Combo_Box  : in Gtk_Combo'Class;
                              Item       : in Gtk.Item.Gtk_Item'Class;
                              Item_Value : in String);
   --  mapping: Set_Item_String gtkcombo.h gtk_combo_set_item_string

   procedure Set_Popdown_Strings (Combo_Box : in Gtk_Combo'Class;
                                  Strings   : in String_List.Glist);
   --  mapping: Set_Popdown_Strings gtkcombo.h gtk_combo_set_popdown_strings

   procedure Set_Use_Arrows (Combo_Box : in Gtk_Combo'Class;
                             Val       : in Boolean);
   --  mapping: Set_Use_Arrows gtkcombo.h gtk_combo_set_use_arrows

   procedure Set_Use_Arrows_Always (Combo_Box : in Gtk_Combo'Class;
                                    Val       : in Boolean);
   --  mapping: Set_Use_Arrows_Always gtkcombo.h \
   --  mapping: gtk_combo_set_use_arrows_always

   procedure Set_Value_In_List (Combo_Box   : in Gtk_Combo'Class;
                                Val         : in Gint;
                                Ok_If_Empty : in Boolean);
   --  mapping: Set_Value_In_List gtkcombo.h gtk_combo_set_value_in_list

   --  mapping: NOT_IMPLEMENTED gtkcombo.h gtk_combo_get_type

private

   type Gtk_Combo is new Gtk.Hbox.Gtk_Hbox with null record;

end Gtk.Combo;


with Gtk.Label;
with Gtk.Widget;

package Gtk.Tips_Query is

   type Gtk_Tips_Query is new Gtk.Label.Gtk_Label with private;

   procedure Gtk_New (Widget : out Gtk_Tips_Query);
   procedure Set_Caller
      (Tips_Query : in Gtk_Tips_Query'Class;
       Caller     : in Gtk.Widget.Gtk_Widget'Class);
   procedure Set_Labels
      (Tips_Query     : in Gtk_Tips_Query'Class;
       Label_Inactive : in String;
       Label_No_Tip   : in String);
   procedure Start_Query (Tips_Query : in out Gtk_Tips_Query'Class);
   procedure Stop_Query (Tips_Query : in out Gtk_Tips_Query'Class);

private
   type Gtk_Tips_Query is new Gtk.Label.Gtk_Label with null record;

   --  mapping: NOT_IMPLEMENTED gtktipsquery.h gtk_tips_query_get_type
   --  mapping: Gtk_New gtktipsquery.h gtk_tips_query_new
   --  mapping: Set_Caller gtktipsquery.h gtk_tips_query_set_caller
   --  mapping: Set_Labels gtktipsquery.h gtk_tips_query_set_labels
   --  mapping: Start_Query gtktipsquery.h gtk_tips_query_start_query
   --  mapping: Stop_Query gtktipsquery.h gtk_tips_query_stop_query
end Gtk.Tips_Query;


with GNAT.Strings;

with Glib;                         use Glib;
with Gdk.Enums;                    use Gdk.Enums;
with Gdk.Event;                    use Gdk.Event;

with Gtk.Widget;
with Pango.Attributes;

with Gtk;                          use Gtk;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Frame;                    use Gtk.Frame;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;

with Gtk.Event_Controller_Key;     use Gtk.Event_Controller_Key;
with Gtk.IM_Context;               use Gtk.IM_Context;
with Gtk.IM_Context_Simple;        use Gtk.IM_Context_Simple;

with Gtkada.Types;                 use Gtkada.Types;

package body Create_Unicode_Entry is

   The_Entry : Gtk_Entry;
   The_Label : Gtk_Label;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "Simple demo of Gtk.Entry that supports unicode characters."
         & ASCII.LF
        & "Use `Ctrl+Shift+U 20AC` to insert the unicode symbol."
        & ASCII.LF
        & "Uses custom Event_Controller_Key and IM_Context_Simple to add custom codes."
        & ASCII.LF
        & "Use combination `\+c+o` for a copyright and `\+c+s` for a smile."
        & ASCII.LF
        & "The preedit string is displayed in the label below";
   end Help;

   -------------
   -- Preedit --
   -------------

   procedure Preedit (Self : access Gtk_IM_Context_Record'Class)
   is
       Str        : Gtkada.Types.Chars_Ptr;
       Attrs      : Pango.Attributes.Pango_Attr_List;
       Cursor_Pos : Glib.Gint;
   begin
      Self.Get_Preedit_String (Str, Attrs, Cursor_Pos);
      declare
         S : constant String := Value (Str);
      begin
         --  preserve the last preedit
         if S /= "" then
            The_Label.Set_Text (Value (Str));
         end if;
      end;

      Free (Str);
      Pango.Attributes.Unref (Attrs);
   end Preedit;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Self : access Gtk_IM_Context_Record'Class;
      Str  : UTF8_String) is
   begin
      The_Entry.Set_Text (The_Entry.Get_Text & Str);
      The_Entry.Set_Position (Gint (The_Entry.Get_Text_Length));
   end Commit;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      VBox                 : Gtk.Box.Gtk_Box;

      Event_Controller_Key : Gtk_Event_Controller_Key;
      IM_Context           : Gtk_IM_Context_Simple;
      Dummy                : Boolean;
   begin
      Set_Label (Frame, "Entry with Unicode");

      Gtk_New (VBox, Orientation_Vertical, 0);
      Frame.Set_Child (VBox);

      The_Entry            := Gtk_Entry_New;
      The_Label            := Gtk_Label_New;
      Event_Controller_Key := Gtk_Event_Controller_Key_New;
      IM_Context           := Gtk_IM_Context_Simple_New;

      IM_Context.Add_Compose_File ("emojis.map");
      IM_Context.On_Preedit_Changed (Preedit'Unrestricted_Access);
      IM_Context.On_Commit (Commit'Unrestricted_Access);

      Event_Controller_Key.Set_Im_Context (IM_Context);

      Event_Controller_Key.Set_Propagation_Phase (Gtk.Enums.Phase_Capture);
      IM_Context.Set_Client_Widget (The_Entry);
      Add_Controller (The_Entry, Event_Controller_Key);

      The_Label.Set_Text ("''");

      VBox.Append (The_Entry);
      VBox.Append (The_Label);

      The_Entry.Show;
      Dummy := The_Entry.Grab_Focus_Without_Selecting;
   end Run;

end Create_Unicode_Entry;

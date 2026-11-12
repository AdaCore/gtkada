--  C-convention callbacks for the GtkIMContext signal tests.
--  These are library-level subprograms so GtkAda can safely store their
--  addresses in GObject closures while the test emits each signal.

with Glib;
with Glib.Object;
with Gtk.IM_Context;

package Imcontext_Handlers is
   use Glib;

   --  Shared capture data keeps callbacks simple and lets tests compare args.
   Current_Context : Gtk.IM_Context.Gtk_IM_Context;
   Retrieve_Anchor : Glib.Gint := 4;
   Context_Void_Calls : Natural := 0;
   Slot_Void_Calls : Natural := 0;
   Context_String : String (1 .. 32) := (others => ' ');
   Slot_String : String (1 .. 32) := (others => ' ');
   Context_String_Length : Natural := 0;
   Slot_String_Length : Natural := 0;
   Last_Offset : Glib.Gint := 0;
   Last_Count : Glib.Gint := 0;

   --  String callback family used by commit and invalid-composition.
   procedure Context_Commit
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Text : UTF8_String);

   procedure Slot_Commit
     (Self : access Glib.Object.GObject_Record'Class;
      Text : UTF8_String);

   function Context_Delete
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Offset, N_Chars : Glib.Gint) return Boolean;

   function Slot_Delete
     (Self : access Glib.Object.GObject_Record'Class;
      Offset, N_Chars : Glib.Gint) return Boolean;

   function Context_Invalid
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class;
      Text : UTF8_String) return Boolean;

   function Slot_Invalid
     (Self : access Glib.Object.GObject_Record'Class;
      Text : UTF8_String) return Boolean;

   procedure Context_Void
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class);

   procedure Slot_Void
     (Self : access Glib.Object.GObject_Record'Class);

   function Context_Retrieve
     (Self : access Gtk.IM_Context.Gtk_IM_Context_Record'Class)
      return Boolean;

   function Slot_Retrieve
     (Self : access Glib.Object.GObject_Record'Class)
      return Boolean;
end Imcontext_Handlers;

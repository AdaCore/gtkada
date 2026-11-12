--  Tests Gtk.Multi_Sorter constructors, list-model accessors, comparison,
--  append/removal behavior, and interface conversions. The main behavior
--  ports testsuite/c_tests/sorter.c (/sorter/multi).

with Ada.Command_Line;
with System;

with Glib;                  use Glib;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gtk.Buildable;
with Gtk.Custom_Sorter;     use Gtk.Custom_Sorter;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Main;
with Gtk.Multi_Sorter;      use Gtk.Multi_Sorter;
with Gtk.Sorter;            use Gtk.Sorter;
with Gtk.String_Object;     use Gtk.String_Object;

use type Glib.GType;
use type Gtk.Multi_Sorter.Gtk_Multi_Sorter;
use type Gtk.Enums.Gtk_Ordering;
use type System.Address;

procedure Multi_Sorter is

   procedure Test_Multi_Sorter with Convention => C;

   --  This primary key groups strings by length; equality lets the secondary
   --  lexical sorter decide the order, as in Gtk.Multi_Sorter's documented
   --  first-non-equal comparison rule.
   function Compare_Length
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   --  The secondary sorter gives deterministic ordering within each length
   --  group, and its C convention matches Gtk.Custom_Sorter's callback ABI.
   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   --------------------
   -- Compare_Length --
   --------------------

   function Compare_Length
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
   begin
      if Left'Length < Right'Length then
         return -1;
      elsif Left'Length > Right'Length then
         return 1;
      else
         return 0;
      end if;
   end Compare_Length;

   --------------------
   -- Compare_Lexical --
   --------------------

   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
   begin
      if Left < Right then
         return -1;
      elsif Left > Right then
         return 1;
      else
         return 0;
      end if;
   end Compare_Lexical;

   -----------------------
   -- Test_Multi_Sorter --
   -----------------------

   procedure Test_Multi_Sorter is
      By_Function : constant Gtk_Multi_Sorter := Gtk_Multi_Sorter_New;
      By_Procedure : Gtk_Multi_Sorter;
      Primary : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Length'Unrestricted_Access);
      Secondary : constant Gtk_Custom_Sorter :=
        Gtk_Custom_Sorter_New (Compare_Lexical'Unrestricted_Access);
      Short : constant Gtk_String_Object := Gtk_String_Object_New ("b");
      Alpha : constant Gtk_String_Object := Gtk_String_Object_New ("aa");
      Beta  : constant Gtk_String_Object := Gtk_String_Object_New ("ab");
      Original_Object : System.Address;
      Model : Glist_Model;
      Buildable : Gtk.Buildable.Gtk_Buildable;
      Item : GObject;
   begin
      --  Both constructor forms produce the sorter GType and begin empty.
      Assert_True (Gtk.Multi_Sorter.Get_Type /= GType_Invalid);
      Assert_Cmpuint_Eq (Get_N_Items (By_Function), 0);
      Gtk_New (By_Procedure);
      Original_Object := Get_Object (By_Procedure);

      --  Reinitializing an existing instance preserves its underlying GObject.
      Gtk.Multi_Sorter.Initialize (By_Procedure);
      Assert_True (Get_Object (By_Procedure) = Original_Object);

      --  Append consumes each sorter's reference; the multi sorter owns them
      --  until removal or its own final unref.
      Append (By_Procedure, Primary);
      Append (By_Procedure, Secondary);
      Assert_Cmpuint_Eq (Get_N_Items (By_Procedure), 2);
      Assert_Cmpuint_Eq
        (Get_Property (By_Procedure, N_Items_Property), 2);
      Assert_True (Get_Item_Type (By_Procedure) = Gtk.Sorter.Get_Type);

      --  Fetching a list item returns a full reference that this test releases.
      Item := Get_Item (By_Procedure, 1);
      Assert_True (Item /= null);
      Assert_True (Get_Object (Item) = Get_Object (Secondary));
      Unref (Item);

      --  Round-trip both interfaces implemented by GtkMultiSorter. The list
      --  model interface also reports the same item type and item count.
      Model := Gtk.Multi_Sorter."+" (By_Procedure);
      Assert_True (Gtk.Multi_Sorter."-" (Model) = By_Procedure);
      Assert_Cmpuint_Eq (Glib.List_Model.Get_N_Items (Model), 2);
      Assert_True
        (Glib.List_Model.Get_Item_Type (Model) = Gtk.Sorter.Get_Type);
      Buildable := Gtk.Multi_Sorter."+" (By_Procedure);
      Assert_True (Gtk.Multi_Sorter."-" (Buildable) = By_Procedure);

      --  The length sorter orders unlike lengths; a tie falls through to the
      --  lexical sorter. These assertions check both stages of the chain.
      Assert_True (Compare (By_Procedure, Short, Alpha) = Smaller);
      Assert_True (Compare (By_Procedure, Alpha, Beta) = Smaller);

      --  Removing an invalid position is a no-op. Removing each valid sorter
      --  updates the list model and changes which comparison key is active.
      Remove (By_Procedure, 42);
      Assert_Cmpuint_Eq (Get_N_Items (By_Procedure), 2);
      Remove (By_Procedure, 0);
      Assert_Cmpuint_Eq (Get_N_Items (By_Procedure), 1);
      Assert_True (Compare (By_Procedure, Alpha, Beta) = Smaller);
      Remove (By_Procedure, 0);
      Assert_Cmpuint_Eq (Get_N_Items (By_Procedure), 0);
      Assert_True (Compare (By_Procedure, Alpha, Beta) = Equal);

      --  Release each independently created GObject after its final use.
      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Beta);
      Unref (Alpha);
      Unref (Short);
   end Test_Multi_Sorter;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/multi-sorter/construct-append-remove",
      Test_Multi_Sorter'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Multi_Sorter;

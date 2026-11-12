--  Exercises Glib.Menu constructors, editing operations, attributes, and links.
--  No C test directly covers GMenu's API; the GtkBuilder GMenu test is outside
--  this binding because GtkBuilder is not bound in this checkout.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Menu;        use Glib.Menu;
with Glib.Menu_Model;  use Glib.Menu_Model;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Glib.Variant;     use Glib.Variant;

procedure Glib_Menu is

   Attribute_Label_Found  : Boolean := False;
   Attribute_Action_Found : Boolean := False;
   Attribute_Custom_Found : Boolean := False;
   Link_Custom_Found     : Boolean := False;
   Link_Section_Found    : Boolean := False;
   Model_Change_Count    : Natural := 0;
   Slot_Change_Count     : Natural := 0;
   Model_Change_Position : Glib.Gint := -1;
   Model_Change_Removed  : Glib.Gint := -1;
   Model_Change_Added    : Glib.Gint := -1;
   Slot_Change_Position  : Glib.Gint := -1;
   Slot_Change_Removed   : Glib.Gint := -1;
   Slot_Change_Added     : Glib.Gint := -1;

   procedure Record_Model_Change
     (Self     : access Gmenu_Model_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint);

   procedure Record_Slot_Change
     (Self     : access GObject_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint);

   --  Keep the signal callbacks small; the test checks their recorded values
   --  after GMenu emits items-changed in response to an append.
   procedure Record_Model_Change
     (Self     : access Gmenu_Model_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint) is
   begin
      Assert_True (Self /= null);
      Model_Change_Count := Model_Change_Count + 1;
      Model_Change_Position := Position;
      Model_Change_Removed := Removed;
      Model_Change_Added := Added;
   end Record_Model_Change;

   --  For a swapped connection the first argument is the supplied slot object.
   procedure Record_Slot_Change
     (Self     : access GObject_Record'Class;
      Position : Glib.Gint;
      Removed  : Glib.Gint;
      Added    : Glib.Gint) is
   begin
      Assert_True (Self /= null);
      Slot_Change_Count := Slot_Change_Count + 1;
      Slot_Change_Position := Position;
      Slot_Change_Removed := Removed;
      Slot_Change_Added := Added;
   end Record_Slot_Change;

   procedure Assert_Item_Attribute
     (Item : Gmenu_Item;
      Name : UTF8_String;
      Text : UTF8_String);

   --  Read the value through the binding and release its owned GVariant.
   procedure Assert_Item_Attribute
     (Item : Gmenu_Item;
      Name : UTF8_String;
      Text : UTF8_String)
   is
      Value : constant Gvariant :=
        Item.Get_Attribute_Value (Name, Gvariant_Type_String);
   begin
      Assert_False (Is_Null (Value));
      Assert_Cmpstr_Eq (Get_String (Value), Text);
      Unref (Value);
   end Assert_Item_Attribute;

   procedure Assert_Model_Label
     (Model : Gmenu_Model;
      Index : Glib.Gint;
      Text  : UTF8_String);

   --  Menu models expose item attributes separately from GMenuItem objects.
   procedure Assert_Model_Label
     (Model : Gmenu_Model;
      Index : Glib.Gint;
      Text  : UTF8_String)
   is
      Value : constant Gvariant :=
        Model.Get_Item_Attribute_Value
          (Index, "label", Gvariant_Type_String);
   begin
      Assert_False (Is_Null (Value));
      Assert_Cmpstr_Eq (Get_String (Value), Text);
      Unref (Value);
   end Assert_Model_Label;

   procedure Test_Constructors with Convention => C;
   procedure Test_Menu_Operations with Convention => C;
   procedure Test_Item_Attributes_And_Links with Convention => C;
   procedure Test_Model_Iterators with Convention => C;
   procedure Test_Items_Changed_Signals with Convention => C;

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      Menu_Out   : Gmenu;
      Menu_Init  : constant Gmenu := new Gmenu_Record;
      Menu_Func  : constant Gmenu := Gmenu_New;
      Source     : constant Gmenu := Gmenu_New;
      Item_Out   : Gmenu_Item;
      Item_Init  : constant Gmenu_Item := new Gmenu_Item_Record;
      Item_Func  : constant Gmenu_Item := Gmenu_Item_New ("initial", "app.initial");
      Copy_Out   : Gmenu_Item;
      Copy_Init  : constant Gmenu_Item := new Gmenu_Item_Record;
      Copy_Func  : Gmenu_Item;
      Section_Out  : Gmenu_Item;
      Section_Init : constant Gmenu_Item := new Gmenu_Item_Record;
      Section_Func : Gmenu_Item;
      Submenu_Out  : Gmenu_Item;
      Submenu_Init : constant Gmenu_Item := new Gmenu_Item_Record;
      Submenu_Func : Gmenu_Item;
   begin
      --  Each menu constructor must produce a usable empty model.
      G_New (Menu_Out);
      Glib.Menu.Initialize (Menu_Init);
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu_Out)), 0);
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu_Init)), 0);
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu_Func)), 0);

      --  Seed a source model so the copy constructors have a valid item index.
      Source.Append ("source", "app.source");
      Copy_Func := Gmenu_Item_New_From_Model (Gmenu_Model (Source), 0);
      Section_Func := Gmenu_Item_New_Section ("section", Gmenu_Model (Source));
      Submenu_Func := Gmenu_Item_New_Submenu ("submenu", Gmenu_Model (Source));
      G_New (Item_Out, "out", "app.out");
      Initialize (Item_Init, "initialized", "app.initialized");
      Assert_Item_Attribute (Item_Out, "label", "out");
      Assert_Item_Attribute (Item_Init, "label", "initialized");
      Assert_Item_Attribute (Item_Func, "label", "initial");

      --  Both procedure and function forms copy the selected source item.
      G_New_From_Model (Copy_Out, Gmenu_Model (Source), 0);
      Initialize_From_Model (Copy_Init, Gmenu_Model (Source), 0);
      Assert_Item_Attribute (Copy_Out, "label", "source");
      Assert_Item_Attribute (Copy_Init, "label", "source");
      Assert_Item_Attribute (Copy_Func, "label", "source");

      --  Section and submenu constructors attach their model as a named link.
      G_New_Section (Section_Out, "section", Gmenu_Model (Source));
      Initialize_Section (Section_Init, "section", Gmenu_Model (Source));
      Assert_True (Section_Out.Get_Link ("section") /= null);
      Assert_True (Section_Init.Get_Link ("section") /= null);
      Assert_True (Section_Func.Get_Link ("section") /= null);
      G_New_Submenu (Submenu_Out, "submenu", Gmenu_Model (Source));
      Initialize_Submenu (Submenu_Init, "submenu", Gmenu_Model (Source));
      Assert_True (Submenu_Out.Get_Link ("submenu") /= null);
      Assert_True (Submenu_Init.Get_Link ("submenu") /= null);
      Assert_True (Submenu_Func.Get_Link ("submenu") /= null);

      --  Release the references returned by all constructors in this case.
      Unref (Menu_Out);
      Unref (Menu_Init);
      Unref (Menu_Func);
      Unref (Source);
      Unref (Item_Out);
      Unref (Item_Init);
      Unref (Item_Func);
      Unref (Copy_Out);
      Unref (Copy_Init);
      Unref (Copy_Func);
      Unref (Section_Out);
      Unref (Section_Init);
      Unref (Section_Func);
      Unref (Submenu_Out);
      Unref (Submenu_Init);
      Unref (Submenu_Func);
   end Test_Constructors;

   -------------------------
   -- Test_Menu_Operations --
   -------------------------

   procedure Test_Menu_Operations is
      Menu    : constant Gmenu := Gmenu_New;
      Section : constant Gmenu := Gmenu_New;
      Submenu : constant Gmenu := Gmenu_New;
      Item    : constant Gmenu_Item := Gmenu_Item_New ("copied", "app.copied");
   begin
      --  The linked models each contain one item, making link queries useful.
      Section.Append ("section-child", "app.section-child");
      Submenu.Append ("submenu-child", "app.submenu-child");

      --  Append variants add ordinary items, copied items, sections, and menus.
      Menu.Append ("appended", "app.appended");
      Menu.Append_Item (Item);
      Menu.Append_Section ("appended-section", Gmenu_Model (Section));
      Menu.Append_Submenu ("appended-submenu", Gmenu_Model (Submenu));
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu)), 4);
      Assert_Model_Label (Gmenu_Model (Menu), 0, "appended");
      Assert_Model_Label (Gmenu_Model (Menu), 1, "copied");
      Assert_True (Get_Item_Link (Gmenu_Model (Menu), 2, "section") /= null);
      Assert_True (Get_Item_Link (Gmenu_Model (Menu), 3, "submenu") /= null);

      --  Insert variants place all four forms at the requested positions.
      Menu.Insert (1, "inserted", "app.inserted");
      Menu.Insert_Item (2, Item);
      Menu.Insert_Section (3, "inserted-section", Gmenu_Model (Section));
      Menu.Insert_Submenu (4, "inserted-submenu", Gmenu_Model (Submenu));
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu)), 8);
      Assert_Model_Label (Gmenu_Model (Menu), 1, "inserted");
      Assert_Model_Label (Gmenu_Model (Menu), 2, "copied");

      --  Prepend variants each put the new entry at index zero.
      Menu.Prepend ("prepended", "app.prepended");
      Assert_Model_Label (Gmenu_Model (Menu), 0, "prepended");
      Menu.Prepend_Item (Item);
      Assert_Model_Label (Gmenu_Model (Menu), 0, "copied");
      Menu.Prepend_Section ("prepended-section", Gmenu_Model (Section));
      Assert_True (Get_Item_Link (Gmenu_Model (Menu), 0, "section") /= null);
      Menu.Prepend_Submenu ("prepended-submenu", Gmenu_Model (Submenu));
      Assert_True (Get_Item_Link (Gmenu_Model (Menu), 0, "submenu") /= null);
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu)), 12);

      --  Remove deletes one indexed entry; Remove_All empties the model.
      Menu.Remove (0);
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu)), 11);
      Menu.Remove_All;
      Assert_Cmpint_Eq (Get_N_Items (Gmenu_Model (Menu)), 0);
      Menu.Append ("frozen", "app.frozen");
      Menu.Freeze;
      Assert_False (Is_Mutable (Gmenu_Model (Menu)));

      Unref (Item);
      Unref (Section);
      Unref (Submenu);
      Unref (Menu);
   end Test_Menu_Operations;

   ------------------------------------
   -- Test_Item_Attributes_And_Links --
   ------------------------------------

   procedure Test_Item_Attributes_And_Links is
      Item    : constant Gmenu_Item := Gmenu_Item_New ("first", "app.first");
      Section : constant Gmenu := Gmenu_New;
      Submenu : constant Gmenu := Gmenu_New;
      Target  : constant Gvariant := Gvariant_New_String ("fast");
      Value   : Gvariant;
      Link    : Gmenu_Model;
   begin
      Section.Append ("section-item", "app.section-item");
      Submenu.Append ("submenu-item", "app.submenu-item");

      --  Query an existing, missing, and wrongly typed attribute.
      Assert_Item_Attribute (Item, "label", "first");
      Value := Item.Get_Attribute_Value ("missing", Gvariant_Type_String);
      Assert_True (Is_Null (Value));
      Value := Item.Get_Attribute_Value ("label", Gvariant_Type_Boolean);
      Assert_True (Is_Null (Value));

      --  Setting a custom attribute stores it; a null variant removes it.
      Item.Set_Attribute_Value ("custom", Gvariant_New_String ("value"));
      Assert_Item_Attribute (Item, "custom", "value");
      Item.Set_Attribute_Value ("custom", Null_Gvariant);
      Value := Item.Get_Attribute_Value ("custom", Gvariant_Type_String);
      Assert_True (Is_Null (Value));

      --  The detailed-action and action/target helpers update standard fields.
      Item.Set_Detailed_Action ("app.save");
      Assert_Item_Attribute (Item, "action", "app.save");
      Item.Set_Action_And_Target_Value ("app.choose", Target);
      Assert_Item_Attribute (Item, "action", "app.choose");
      Assert_Item_Attribute (Item, "target", "fast");
      Item.Set_Action_And_Target_Value ("", Null_Gvariant);
      Value := Item.Get_Attribute_Value ("action", Gvariant_Type_String);
      Assert_True (Is_Null (Value));
      Value := Item.Get_Attribute_Value ("target", Gvariant_Type_String);
      Assert_True (Is_Null (Value));
      --  GLib sinks the target's floating reference into the menu item.

      --  Empty labels and null icons clear their corresponding attributes.
      Item.Set_Label ("renamed");
      Assert_Item_Attribute (Item, "label", "renamed");
      Item.Set_Label ("");
      Value := Item.Get_Attribute_Value ("label", Gvariant_Type_String);
      Assert_True (Is_Null (Value));
      Item.Set_Icon (null);
      Value := Item.Get_Attribute_Value ("icon", Gvariant_Type_String);
      Assert_True (Is_Null (Value));

      --  Generic links can be set, read, and unset through the Ada binding.
      Item.Set_Link ("custom-link", Gmenu_Model (Submenu));
      Link := Item.Get_Link ("custom-link");
      Assert_True (Link /= null);
      Assert_Cmpint_Eq (Get_N_Items (Link), 1);
      Unref (Link);
      Item.Set_Link ("custom-link", null);
      Assert_True (Item.Get_Link ("custom-link") = null);

      --  The convenience setters use the standard section and submenu links.
      Item.Set_Section (Gmenu_Model (Section));
      Link := Item.Get_Link ("section");
      Assert_True (Link /= null);
      Assert_Cmpint_Eq (Get_N_Items (Link), 1);
      Unref (Link);
      Item.Set_Section (null);
      Assert_True (Item.Get_Link ("section") = null);
      Item.Set_Submenu (Gmenu_Model (Submenu));
      Link := Item.Get_Link ("submenu");
      Assert_True (Link /= null);
      Assert_Cmpint_Eq (Get_N_Items (Link), 1);
      Unref (Link);
      Item.Set_Submenu (null);
      Assert_True (Item.Get_Link ("submenu") = null);

      Unref (Item);
      Unref (Section);
      Unref (Submenu);
   end Test_Item_Attributes_And_Links;

   --------------------------
   -- Test_Model_Iterators --
   --------------------------

   procedure Test_Model_Iterators is
      Model       : constant Gmenu := Gmenu_New;
      Section     : constant Gmenu := Gmenu_New;
      Submenu     : constant Gmenu := Gmenu_New;
      Item        : constant Gmenu_Item := Gmenu_Item_New ("iterated", "app.iterated");
      Attributes  : Gmenu_Attribute_Iter;
      Links       : Gmenu_Link_Iter;
      Value       : Gvariant;
      Linked_Model : Gmenu_Model;
   begin
      --  Add more than the standard label and action so the generic iterator
      --  returns several attributes and the test can check named values.
      Item.Set_Attribute_Value ("custom", Gvariant_New_String ("extra"));
      Item.Set_Link ("custom-link", Gmenu_Model (Submenu));
      Item.Set_Section (Gmenu_Model (Section));
      Model.Append_Item (Item);

      --  Next advances to each attribute; Get_Name and Get_Value inspect the
      --  current entry without advancing it. Returned variants are owned.
      Attributes := Iterate_Item_Attributes (Gmenu_Model (Model), 0);
      while Attributes.Next loop
         declare
            Name : constant UTF8_String := Attributes.Get_Name;
         begin
            Value := Attributes.Get_Value;
            if Name = "label" then
               Attribute_Label_Found := Get_String (Value) = "iterated";
            elsif Name = "action" then
               Attribute_Action_Found := Get_String (Value) = "app.iterated";
            elsif Name = "custom" then
               Attribute_Custom_Found := Get_String (Value) = "extra";
            end if;
            Unref (Value);
         end;
      end loop;
      Assert_True (Attribute_Label_Found);
      Assert_True (Attribute_Action_Found);
      Assert_True (Attribute_Custom_Found);
      Unref (Attributes);

      --  Link iteration returns both named links and a full reference to each
      --  linked model, which this test releases after checking its contents.
      Links := Iterate_Item_Links (Gmenu_Model (Model), 0);
      while Links.Next loop
         declare
            Name : constant UTF8_String := Links.Get_Name;
         begin
            Linked_Model := Links.Get_Value;
            Assert_Cmpint_Eq (Get_N_Items (Linked_Model), 0);
            if Name = "custom-link" then
               Link_Custom_Found := True;
            elsif Name = "section" then
               Link_Section_Found := True;
            end if;
            Unref (Linked_Model);
         end;
      end loop;
      Assert_True (Link_Custom_Found);
      Assert_True (Link_Section_Found);
      Unref (Links);

      Unref (Item);
      Unref (Section);
      Unref (Submenu);
      Unref (Model);
   end Test_Model_Iterators;

   --------------------------------
   -- Test_Items_Changed_Signals --
   --------------------------------

   procedure Test_Items_Changed_Signals is
      Model : constant Gmenu := Gmenu_New;
      Slot  : constant Gmenu := Gmenu_New;
   begin
      --  Register both overloads: the typed callback receives the menu model,
      --  while the slot form receives the supplied GObject as its first arg.
      Model.On_Items_Changed (Record_Model_Change'Unrestricted_Access);
      Model.On_Items_Changed
        (Record_Slot_Change'Unrestricted_Access,
         Slot,
         After => True);

      --  GMenu emits the documented (position, removed, added) signal when an
      --  ordinary public operation changes its model.
      Model.Append ("signal-item", "app.signal-item");
      Assert_Cmpuint_Eq (Glib.Guint (Model_Change_Count), 1);
      Assert_Cmpint_Eq (Model_Change_Position, 0);
      Assert_Cmpint_Eq (Model_Change_Removed, 0);
      Assert_Cmpint_Eq (Model_Change_Added, 1);
      Assert_Cmpuint_Eq (Glib.Guint (Slot_Change_Count), 1);
      Assert_Cmpint_Eq (Slot_Change_Position, 0);
      Assert_Cmpint_Eq (Slot_Change_Removed, 0);
      Assert_Cmpint_Eq (Slot_Change_Added, 1);

      Unref (Slot);
      Unref (Model);
   end Test_Items_Changed_Signals;

begin
   Glib.Test.Init;
   Add_Func ("/glib/menu/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/glib/menu/operations", Test_Menu_Operations'Unrestricted_Access);
   Add_Func
     ("/glib/menu/item-attributes-links",
      Test_Item_Attributes_And_Links'Unrestricted_Access);
   Add_Func ("/glib/menu/model-iterators", Test_Model_Iterators'Unrestricted_Access);
   Add_Func
     ("/glib/menu/items-changed-signals",
      Test_Items_Changed_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Glib_Menu;

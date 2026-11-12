--  Exercise Pango.Context, Pango.Font_Map, and Pango.Font_Family operations.
--  The C testsuite has no matching Pango tests to port, so these checks cover
--  the documented behavior through the Ada bindings.

with Ada.Command_Line;

with Glib.Object;
with Glib.Test; use Glib.Test;
with Gtk.Label; use Gtk.Label;
with Gtk.Main;
with Pango.Context;
with Pango.Enums;
with Pango.Font; use Pango.Font;
with Pango.Font_Face;
with Pango.Font_Family;
with Pango.Font_Map;
with Pango.Font_Metrics;
with Pango.Fontset;
with Pango.Language;
with Pango.Matrix;

procedure Main is

   use type Glib.Guint;
   use type Glib.Gint;
   use type Pango.Context.Pango_Context;
   use type Pango.Enums.Direction;
   use type Pango.Enums.Gravity;
   use type Pango.Enums.GravityHint;
   use type Pango.Font.Pango_Font;
   use type Pango.Font_Map.Pango_Font_Map;
   use type Pango.Font_Family.Pango_Font_Family;
   use type Pango.Fontset.Pango_Fontset;

   --  Keep callback observations outside the C-to-Ada callback frames so all
   --  test assertions run after Pango has returned to the test procedure.
   Active_Fontset                  : Pango.Fontset.Pango_Fontset;
   Plain_Foreach_Count             : Glib.Gint := 0;
   Plain_Foreach_Saw_Font          : Boolean := False;
   Plain_Foreach_Saw_Fontset       : Boolean := False;
   Data_Foreach_Count              : Glib.Gint := 0;
   Data_Foreach_Saw_Font           : Boolean := False;
   Data_Foreach_Saw_Value          : Boolean := False;
   Data_Foreach_Destroyed          : Boolean := False;

   --  The generic callback lets the test also verify cleanup of its copied
   --  user data after the Pango iteration completes.
   procedure Destroy_Foreach_Data (Data : in out Integer);
   package Fontset_Foreach_With_Data is new Pango.Fontset.Foreach_User_Data
     (Integer, Destroy_Foreach_Data);

   function Stop_At_First_Font
     (Fontset : not null access Pango.Fontset.Pango_Fontset_Record'Class;
      Font    : not null access Pango.Font.Pango_Font_Record'Class)
      return Boolean;
   function Continue_With_Data
     (Fontset   : not null access Pango.Fontset.Pango_Fontset_Record'Class;
      Font      : not null access Pango.Font.Pango_Font_Record'Class;
      User_Data : Integer)
      return Boolean;

   -------------------------
   -- Destroy_Foreach_Data --
   -------------------------

   --  The closure owns a copy of the value, so this marks that its destroy
   --  hook ran after the callback completed.
   procedure Destroy_Foreach_Data (Data : in out Integer) is
   begin
      Data_Foreach_Destroyed := Data = 1729;
   end Destroy_Foreach_Data;

   -------------------------
   -- Stop_At_First_Font --
   -------------------------

   --  Returning True checks Pango's documented early-stop behavior.
   function Stop_At_First_Font
     (Fontset : not null access Pango.Fontset.Pango_Fontset_Record'Class;
      Font    : not null access Pango.Font.Pango_Font_Record'Class)
      return Boolean
   is
   begin
      Plain_Foreach_Count := Plain_Foreach_Count + 1;
      Plain_Foreach_Saw_Font := Font /= null;
      Plain_Foreach_Saw_Fontset :=
        Pango.Fontset.Pango_Fontset (Fontset) = Active_Fontset;
      return True;
   end Stop_At_First_Font;

   -------------------------
   -- Continue_With_Data --
   -------------------------

   --  Returning False visits the whole set and checks callback data delivery.
   function Continue_With_Data
     (Fontset   : not null access Pango.Fontset.Pango_Fontset_Record'Class;
      Font      : not null access Pango.Font.Pango_Font_Record'Class;
      User_Data : Integer)
      return Boolean
   is
   begin
      Data_Foreach_Count := Data_Foreach_Count + 1;
      Data_Foreach_Saw_Font := Font /= null;
      Data_Foreach_Saw_Value := User_Data = 1729;
      return False;
   end Continue_With_Data;

   procedure Test_Context
   with Convention => C;

   procedure Test_Font_Map
   with Convention => C;

   procedure Test_Font_Families
   with Convention => C;

   ------------------
   -- Test_Context --
   ------------------

   --  Use a GTK-created context to ensure font lookup runs against the
   --  installed, deterministic headless font map available under Xvfb.
   procedure Test_Context is
      Label          : Gtk_Label;
      Context        : Pango.Context.Pango_Context;
      New_Context    : Pango.Context.Pango_Context;
      Function_New   : Pango.Context.Pango_Context;
      Font_Map       : Pango.Font_Map.Pango_Font_Map;
      Description    : Pango_Font_Description := From_String ("Sans 12");
      Read_Desc      : Pango_Font_Description;
      Language       : constant Pango.Language.Pango_Language :=
        Pango.Language.From_String ("fr");
      Read_Language  : Pango.Language.Pango_Language;
      Matrix         : constant Pango.Matrix.Pango_Matrix :=
        (Xx => 1.25, Xy => 0.0, Yx => 0.0, Yy => 1.5, X0 => 2.0, Y0 => 3.0);
      Read_Matrix    : Pango.Matrix.Pango_Matrix;
      Font           : Pango.Font.Pango_Font;
      Fontset_Font   : Pango.Font.Pango_Font;
      Font_Desc      : Pango.Font.Pango_Font_Description;
      Fontset        : Pango.Fontset.Pango_Fontset;
      Metrics        : Pango.Font_Metrics.Pango_Font_Metrics;
      Serial         : Glib.Guint;
   begin
      Gtk_New (Label, "context font map");
      Context := Label.Get_Pango_Context;
      Font_Map := Context.Get_Font_Map;
      Assert_True (Font_Map /= null);

      --  Both constructors initialize a usable object. Re-initializing an
      --  already-created GObject must leave it intact.
      Pango.Context.Gdk_New (New_Context);
      Pango.Context.Initialize (New_Context);
      New_Context.Set_Font_Map (Font_Map);
      Assert_True (New_Context.Get_Font_Map = Font_Map);
      Function_New := Pango.Context.Pango_Context_New;
      Function_New.Set_Font_Map (Font_Map);
      Assert_True (Function_New.Get_Font_Map = Font_Map);

      --  Direction, gravity, hint, language, and rounding setters are
      --  observable through their matching getters.
      Context.Set_Base_Dir (Pango.Enums.Pango_Direction_Rtl);
      Assert_True
        (Context.Get_Base_Dir = Pango.Enums.Pango_Direction_Rtl);
      Context.Set_Base_Gravity (Pango.Enums.Pango_Gravity_East);
      Assert_True
        (Context.Get_Base_Gravity = Pango.Enums.Pango_Gravity_East);
      Assert_True (Context.Get_Gravity = Pango.Enums.Pango_Gravity_East);
      Context.Set_Gravity_Hint (Pango.Enums.Pango_Gravity_Hint_Strong);
      Assert_True
        (Context.Get_Gravity_Hint = Pango.Enums.Pango_Gravity_Hint_Strong);
      Context.Set_Language (Language);
      Read_Language := Context.Get_Language;
      Assert_True (Pango.Language.To_String (Read_Language) = "fr");
      Context.Set_Round_Glyph_Positions (False);
      Assert_False (Context.Get_Round_Glyph_Positions);
      Context.Set_Round_Glyph_Positions (True);
      Assert_True (Context.Get_Round_Glyph_Positions);

      --  The description getter returns the context's current default font.
      Context.Set_Font_Description (Description);
      Read_Desc := Context.Get_Font_Description;
      Assert_True (Get_Family (Read_Desc) = "Sans");

      --  Supply a matrix before reading it: Pango returns null when no
      --  matrix is set, which this record-valued Ada binding cannot express.
      Context.Set_Matrix (Matrix);
      Read_Matrix := Context.Get_Matrix;
      Assert_Cmpfloat_Eq (Read_Matrix.Xx, 1.25);
      Assert_Cmpfloat_Eq (Read_Matrix.Yy, 1.5);
      Assert_Cmpfloat_Eq (Read_Matrix.X0, 2.0);
      Assert_Cmpfloat_Eq (Read_Matrix.Y0, 3.0);

      --  Font families are returned as borrowed family objects in an Ada
      --  array. A populated list verifies the C out-parameters and conversion.
      declare
         Families : constant Pango.Font_Family.Pango_Font_Family_Array :=
           Context.List_Families;
      begin
         Assert_Cmpuint_Gt (Glib.Guint (Families'Length), 0);
         Assert_True
           (Pango.Font_Family.Get_Name (Families (Families'First))'Length > 0);
      end;

      --  Font, fontset, and metrics queries all use the configured font map.
      Font := Context.Load_Font (Description);
      Assert_True (Font /= null);
      Fontset := Context.Load_Fontset (Description, Language);
      Assert_True (Fontset /= null);

      --  Get_Font returns an owned font selected for this character; describe
      --  it to confirm the returned object is usable before releasing it.
      Fontset_Font := Fontset.Get_Font (Glib.Guint (Character'Pos ('A')));
      Assert_True (Fontset_Font /= null);
      Font_Desc := Fontset_Font.Describe;
      Assert_True (Get_Family (Font_Desc)'Length > 0);
      Free (Font_Desc);
      Glib.Object.Unref (Glib.Object.GObject (Fontset_Font));

      --  Fontset metrics are separately owned from context metrics.
      Metrics := Fontset.Get_Metrics;
      Assert_Cmpint_Gt (Pango.Font_Metrics.Get_Ascent (Metrics), 0);
      Pango.Font_Metrics.Unref (Metrics);

      --  The plain callback stops after one font; the data callback continues
      --  through the set and verifies both its payload and destroy hook.
      Active_Fontset := Fontset;
      Plain_Foreach_Count := 0;
      Plain_Foreach_Saw_Font := False;
      Plain_Foreach_Saw_Fontset := False;
      Fontset.Foreach (Stop_At_First_Font'Unrestricted_Access);
      Assert_Cmpint_Eq (Plain_Foreach_Count, 1);
      Assert_True (Plain_Foreach_Saw_Font);
      Assert_True (Plain_Foreach_Saw_Fontset);

      Data_Foreach_Count := 0;
      Data_Foreach_Saw_Font := False;
      Data_Foreach_Saw_Value := False;
      Data_Foreach_Destroyed := False;
      Fontset_Foreach_With_Data.Foreach
        (Fontset, Continue_With_Data'Unrestricted_Access, 1729);
      Assert_Cmpint_Gt (Data_Foreach_Count, 0);
      Assert_True (Data_Foreach_Saw_Font);
      Assert_True (Data_Foreach_Saw_Value);
      Assert_True (Data_Foreach_Destroyed);
      Active_Fontset := null;

      Metrics := Context.Get_Metrics (Description, Language);
      Assert_True (Pango.Font_Metrics.Get_Ascent (Metrics) > 0);
      Assert_True (Pango.Font_Metrics.Get_Descent (Metrics) >= 0);
      Pango.Font_Metrics.Unref (Metrics);

      --  Changed advances the context serial so clients can invalidate cached
      --  layout data after backend state changes.
      Serial := Context.Get_Serial;
      Context.Changed;
      Assert_True (Context.Get_Serial /= Serial);

      --  Release objects with full-transfer ownership and owned description.
      Glib.Object.Unref (Glib.Object.GObject (Font));
      Glib.Object.Unref (Glib.Object.GObject (Fontset));
      Glib.Object.Unref (Glib.Object.GObject (New_Context));
      Glib.Object.Unref (Glib.Object.GObject (Function_New));
      Free (Description);
   end Test_Context;

   -------------------
   -- Test_Font_Map --
   -------------------

   --  Exercise font-map methods directly, using the headless GTK font map
   --  and checking the returned context, families, fonts, and serial.
   procedure Test_Font_Map is
      Label       : Gtk_Label;
      Context     : Pango.Context.Pango_Context;
      Map_Context : Pango.Context.Pango_Context;
      Font_Map    : Pango.Font_Map.Pango_Font_Map;
      Description : Pango_Font_Description := From_String ("Sans 12");
      Language    : constant Pango.Language.Pango_Language :=
        Pango.Language.From_String ("en");
      Font        : Pango.Font.Pango_Font;
      Fontset     : Pango.Fontset.Pango_Fontset;
      Set_Font    : Pango.Font.Pango_Font;
      Serial      : Glib.Guint;
   begin
      --  GTK provides a backend map with installed fonts in the test Xvfb.
      Gtk_New (Label, "font map operations");
      Context := Label.Get_Pango_Context;
      Font_Map := Context.Get_Font_Map;
      Assert_True (Font_Map /= null);

      --  The map creates a new context connected to itself.
      Map_Context := Font_Map.Create_Context;
      Assert_True (Map_Context /= null);
      Assert_True (Map_Context.Get_Font_Map = Font_Map);

      --  The font list conversion returns borrowed family objects in an Ada
      --  array; installed headless fonts make the list non-empty.
      declare
         Families : constant Pango.Font_Family.Pango_Font_Family_Array :=
           Font_Map.List_Families;
      begin
         Assert_Cmpuint_Gt (Glib.Guint (Families'Length), 0);
         Assert_True
           (Pango.Font_Family.Get_Name (Families (Families'First))'Length > 0);
      end;

      --  Both lookup methods resolve the same known description via the map.
      Font := Font_Map.Load_Font (Map_Context, Description);
      Assert_True (Font /= null);
      Assert_True (Pango.Font.Get_Font_Map (Font) = Font_Map);
      Fontset := Font_Map.Load_Fontset (Map_Context, Description, Language);
      Assert_True (Fontset /= null);
      Set_Font := Fontset.Get_Font (Glib.Guint (Character'Pos ('A')));
      Assert_True (Set_Font /= null);
      Assert_True (Pango.Font.Get_Font_Map (Set_Font) = Font_Map);

      --  Changed invalidates cached map data and advances its nonzero serial.
      Serial := Font_Map.Get_Serial;
      Assert_Cmpuint_Gt (Serial, 0);
      Font_Map.Changed;
      Assert_True (Font_Map.Get_Serial /= Serial);

      --  The contexts, fonts, fontset, and description have full ownership.
      Glib.Object.Unref (Glib.Object.GObject (Set_Font));
      Glib.Object.Unref (Glib.Object.GObject (Fontset));
      Glib.Object.Unref (Glib.Object.GObject (Font));
      Glib.Object.Unref (Glib.Object.GObject (Map_Context));
      Free (Description);
   end Test_Font_Map;

   ------------------------
   -- Test_Font_Families --
   ------------------------

   --  Check family classification and the conversion of a family's borrowed
   --  face list. DejaVu Sans is the installed static Sans face in this
   --  headless setup; a monospace family is found from the font map itself.
   procedure Test_Font_Families is
      Label       : Gtk_Label;
      Font_Map    : Pango.Font_Map.Pango_Font_Map;
   begin
      --  GTK supplies the deterministic headless font map used by the other
      --  Pango tests, and family objects in its list are borrowed references.
      Gtk_New (Label, "font family operations");
      Font_Map := Label.Get_Pango_Context.Get_Font_Map;
      declare
         Families : constant Pango.Font_Family.Pango_Font_Family_Array :=
           Font_Map.List_Families;
         Mono_Family : Pango.Font_Family.Pango_Font_Family := null;
         Sans_Family : Pango.Font_Family.Pango_Font_Family := null;
      begin
         Assert_Cmpuint_Gt (Glib.Guint (Families'Length), 0);

         --  Locate a family known to be monospace and the installed static
         --  Sans family for documented expected Boolean query results.
         for Family of Families loop
            if Pango.Font_Family.Is_Monospace (Family) then
               Mono_Family := Family;
            end if;
            if Pango.Font_Family.Get_Name (Family) = "DejaVu Sans" then
               Sans_Family := Family;
            end if;
         end loop;
         Assert_True (Mono_Family /= null);
         Assert_True (Sans_Family /= null);
         Assert_True (Pango.Font_Family.Is_Monospace (Mono_Family));
         Assert_False (Pango.Font_Family.Is_Variable (Sans_Family));

         --  List_Faces returns borrowed face objects in an Ada array. Its
         --  nonempty result exercises the C out-parameters, pointer
         --  conversion, iteration, and release of the temporary C array.
         declare
            Faces : constant Pango.Font_Face.Pango_Font_Face_Array :=
              Pango.Font_Family.List_Faces (Mono_Family);
         begin
            Assert_Cmpuint_Gt (Glib.Guint (Faces'Length), 0);
            Assert_True
              (Pango.Font_Face.Get_Face_Name (Faces (Faces'First))'Length > 0);
            Assert_True
              (Pango.Font_Family.Get_Name
                 (Pango.Font_Face.Get_Family (Faces (Faces'First))) =
               Pango.Font_Family.Get_Name (Mono_Family));
         end;
      end;
   end Test_Font_Families;

begin
   Glib.Test.Init;
   --  GTK must be initialized before a widget can supply its Pango context.
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/pango-context/configuration-and-fonts",
                       Test_Context'Unrestricted_Access);
   Glib.Test.Add_Func ("/pango-context/font-map-operations",
                       Test_Font_Map'Unrestricted_Access);
   Glib.Test.Add_Func ("/pango-context/font-families",
                       Test_Font_Families'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;

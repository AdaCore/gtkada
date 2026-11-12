--  Exercises Gdk.Toplevel_Layout construction, state access, copying,
--  equality, and explicit reference management. No C test covers this API.

with Ada.Command_Line;

with Gdk.Monitor;
with Gdk.Toplevel_Layout;
with Glib;
with Glib.Test;

procedure Main is
   use type Gdk.Monitor.Gdk_Monitor;
   use type Glib.GType;

   procedure Test_Layout_State with Convention => C;

   --  The getters report whether a state was specified separately from its
   --  Boolean value, so check both after setting each state explicitly.
   procedure Test_Layout_State is
      Layout       : Gdk.Toplevel_Layout.Gdk_Toplevel_Layout;
      Other        : Gdk.Toplevel_Layout.Gdk_Toplevel_Layout;
      Copied       : Gdk.Toplevel_Layout.Gdk_Toplevel_Layout;
      Shared_Ref   : Gdk.Toplevel_Layout.Gdk_Toplevel_Layout;
      Fullscreen   : Boolean := False;
      Maximized    : Boolean := False;
   begin
      --  Cover both public constructors; the layout is a boxed value and
      --  each newly created value is released explicitly at the end.
      Gdk.Toplevel_Layout.Gdk_New (Layout);
      Other := Gdk.Toplevel_Layout.Gdk_Toplevel_Layout_New;
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Type /= Glib.GType_Invalid);

      --  A fresh layout does not specify fullscreen or maximized state.
      Glib.Test.Assert_False
        (Gdk.Toplevel_Layout.Get_Fullscreen (Layout, Fullscreen));
      Glib.Test.Assert_False
        (Gdk.Toplevel_Layout.Get_Maximized (Layout, Maximized));

      --  Null is a valid fullscreen monitor: it asks GDK to choose one.
      Gdk.Toplevel_Layout.Set_Fullscreen (Layout, True, null);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Fullscreen (Layout, Fullscreen));
      Glib.Test.Assert_True (Fullscreen);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Fullscreen_Monitor (Layout) = null);
      Gdk.Toplevel_Layout.Set_Fullscreen (Layout, False, null);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Fullscreen (Layout, Fullscreen));
      Glib.Test.Assert_False (Fullscreen);

      --  Maximized has the same "specified" result plus an output value.
      Gdk.Toplevel_Layout.Set_Maximized (Layout, True);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Maximized (Layout, Maximized));
      Glib.Test.Assert_True (Maximized);
      Gdk.Toplevel_Layout.Set_Maximized (Layout, False);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Get_Maximized (Layout, Maximized));
      Glib.Test.Assert_False (Maximized);

      --  Resizability is always a Boolean property, without a separate
      --  "specified" result.
      Gdk.Toplevel_Layout.Set_Resizable (Layout, False);
      Glib.Test.Assert_False (Gdk.Toplevel_Layout.Get_Resizable (Layout));
      Gdk.Toplevel_Layout.Set_Resizable (Layout, True);
      Glib.Test.Assert_True (Gdk.Toplevel_Layout.Get_Resizable (Layout));

      --  Copy preserves all state. Changing the copy leaves the original
      --  unchanged, which also exercises the equality comparison.
      Copied := Gdk.Toplevel_Layout.Copy (Layout);
      Glib.Test.Assert_True (Gdk.Toplevel_Layout.Equal (Layout, Copied));
      Gdk.Toplevel_Layout.Set_Resizable (Copied, False);
      Glib.Test.Assert_False (Gdk.Toplevel_Layout.Equal (Layout, Copied));
      Glib.Test.Assert_True (Gdk.Toplevel_Layout.Get_Resizable (Layout));
      Gdk.Toplevel_Layout.Set_Resizable (Copied, True);
      Glib.Test.Assert_True (Gdk.Toplevel_Layout.Equal (Layout, Copied));

      --  Ref returns another owned reference to the same boxed layout.
      Shared_Ref := Gdk.Toplevel_Layout.Ref (Layout);
      Glib.Test.Assert_True
        (Gdk.Toplevel_Layout.Equal (Layout, Shared_Ref));
      Gdk.Toplevel_Layout.Unref (Shared_Ref);
      Gdk.Toplevel_Layout.Unref (Copied);
      Gdk.Toplevel_Layout.Unref (Other);
      Gdk.Toplevel_Layout.Unref (Layout);
   end Test_Layout_State;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk-toplevel-layout/state", Test_Layout_State'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;

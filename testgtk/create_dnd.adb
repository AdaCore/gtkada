------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib;          use Glib;
with Glib.Values;   use Glib.Values;
with Gtk.Box;       use Gtk.Box;
with Gtk.Dnd;       use Gtk.Dnd;
with Gtk.Target_List; use Gtk.Target_List;
with Gdk.Types;     use Gdk.Types;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Button;    use Gtk.Button;
with Gtk.Grid;      use Gtk.Grid;
with Gtk.Label;     use Gtk.Label;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Image;     use Gtk.Image;
with Gtkada.Types;  use Gtkada.Types;
with Gtk.Widget;    use Gtk.Widget;
with Interfaces.C.Strings;
with Gdk.Dnd;       use Gdk.Dnd;
with Gdk.Drag_Contexts; use Gdk.Drag_Contexts;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Gdk.Window;    use Gdk.Window;
with Gtk.Frame;     use Gtk.Frame;
with Gtk.Selection_Data; use Gtk.Selection_Data;

package body Create_Dnd is

   function "+" (S : String) return Gtkada.Types.Chars_Ptr
                renames Gtkada.Types.New_String;

   Drag_Icon_Xpm : constant Gtkada.Types.Chars_Ptr_Array
     := "36 48 9 1"
     + "       c None"
     + ".      c #020204"
     + "+      c #8F8F90"
     + "@      c #D3D3D2"
     + "#      c #AEAEAC"
     + "$      c #ECECEC"
     + "%      c #A2A2A4"
     + "&      c #FEFEFC"
     + "*      c #BEBEBC"
     + "               ....................."
     + "              ..&&&&&&&&&&&&&&&&&&&."
     + "             ...&&&&&&&&&&&&&&&&&&&."
     + "            ..&.&&&&&&&&&&&&&&&&&&&."
     + "           ..&&.&&&&&&&&&&&&&&&&&&&."
     + "          ..&&&.&&&&&&&&&&&&&&&&&&&."
     + "         ..&&&&.&&&&&&&&&&&&&&&&&&&."
     + "        ..&&&&&.&&&@&&&&&&&&&&&&&&&."
     + "       ..&&&&&&.*$%$+$&&&&&&&&&&&&&."
     + "      ..&&&&&&&.%$%$+&&&&&&&&&&&&&&."
     + "     ..&&&&&&&&.#&#@$&&&&&&&&&&&&&&."
     + "    ..&&&&&&&&&.#$**#$&&&&&&&&&&&&&."
     + "   ..&&&&&&&&&&.&@%&%$&&&&&&&&&&&&&."
     + "  ..&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&."
     + " ..&&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&."
     + "................&$@&&&@&&&&&&&&&&&&."
     + ".&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&&&&&."
     + ".&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&&&."
     + ".&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&&&."
     + ".&&&&&&@#@@$&*@&@#@#$**#$&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&@%&%$&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&+&$+&$&@&$@&&$@&&&&&&&&&&."
     + ".&&&&&&&&&+&&#@%#+@#@*$%&+$&&&&&&&&."
     + ".&&&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&."
     + ".&&&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&."
     + ".&&&&&&&&@#@@$&*@&@#@#$#*#$&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&&&."
     + ".&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&&&&&."
     + ".&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&."
     + ".&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&&&&&."
     + ".&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&&&&&."
     + ".&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&."
     + ".&&&&&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&."
     + ".&&&&&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&."
     + ".&&&&&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&."
     + ".&&&&&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + ".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&."
     + "....................................";

   Trashcan_Closed_Xpm : constant Gtkada.Types.Chars_Ptr_Array
     := "64 80 17 1"
     + "        c None"
     + ".       c #030304"
     + "+       c #5A5A5C"
     + "@       c #323231"
     + "#       c #888888"
     + "$       c #1E1E1F"
     + "%       c #767677"
     + "&       c #494949"
     + "* c #9E9E9C"
     + "= c #111111"
     + "- c #3C3C3D"
     + "; c #6B6B6B"
     + "> c #949494"
     + ", c #282828"
     + "' c #808080"
     + ") c #545454"
     + "! c #AEAEAC"
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                       ==......=$$...===                        "
     + "                 ..$------)+++++++++++++@$$...                  "
     + "             ..=@@-------&+++++++++++++++++++-....              "
     + "          =.$$@@@-&&)++++)-,$$$$=@@&+++++++++++++,..$           "
     + "         .$$$$@@&+++++++&$$$@@@@-&,$,-++++++++++;;;&..          "
     + "        $$$$,@--&++++++&$$)++++++++-,$&++++++;%%'%%;;$@         "
     + "       .-@@-@-&++++++++-@++++++++++++,-++++++;''%;;;%*-$        "
     + "       +------++++++++++++++++++++++++++++++;;%%%;;##*!.        "
     + "        =+----+++++++++++++++++++++++;;;;;;;;;;;;%'>>).         "
     + "         .=)&+++++++++++++++++;;;;;;;;;;;;;;%''>>#>#@.          "
     + "          =..=&++++++++++++;;;;;;;;;;;;;%###>>###+%==           "
     + "           .&....=-+++++%;;####''''''''''##'%%%)..#.            "
     + "           .+-++@....=,+%#####'%%%%%%%%%;@$-@-@*++!.            "
     + "           .+-++-+++-&-@$$=$=......$,,,@;&)+!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            "
     + "            $.++-+++-+++++++++!++++!++++!+++!++!+.$             "
     + "              =.++++++++++++++!++++!++++!+++!++.=               "
     + "                 $..+++++++++++++++!++++++...$                  "
     + "                      $$=.............=$$                       "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                ";

   Trashcan_Open_Xpm : constant Gtkada.Types.Chars_Ptr_Array :=
     "64 80 17 1"
     + "        c None"
     + ".       c #030304"
     + "+       c #5A5A5C"
     + "@       c #323231"
     + "#       c #888888"
     + "$       c #1E1E1F"
     + "%       c #767677"
     + "&       c #494949"
     + "*       c #9E9E9C"
     + "=       c #111111"
     + "-       c #3C3C3D"
     + ";       c #6B6B6B"
     + ">       c #949494"
     + ",       c #282828"
     + "'       c #808080"
     + ")       c #545454"
     + "!       c #AEAEAC"
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                      .=.==.,@                  "
     + "                                   ==.,@-&&&)-=                 "
     + "                                 .$@,&++;;;%>*-                 "
     + "                               $,-+)+++%%;;'#+.                 "
     + "                            =---+++++;%%%;%##@.                 "
     + "                           @)++++++++;%%%%'#%$                  "
     + "                         $&++++++++++;%%;%##@=                  "
     + "                       ,-++++)+++++++;;;'#%)                    "
     + "                      @+++&&--&)++++;;%'#'-.                    "
     + "                    ,&++-@@,,,,-)++;;;'>'+,                     "
     + "                  =-++&@$@&&&&-&+;;;%##%+@                      "
     + "                =,)+)-,@@&+++++;;;;%##%&@                       "
     + "               @--&&,,@&)++++++;;;;'#)@                         "
     + "              ---&)-,@)+++++++;;;%''+,                          "
     + "            $--&)+&$-+++++++;;;%%'';-                           "
     + "           .,-&+++-$&++++++;;;%''%&=                            "
     + "          $,-&)++)-@++++++;;%''%),                              "
     + "         =,@&)++++&&+++++;%'''+$@&++++++                        "
     + "        .$@-++++++++++++;'#';,........=$@&++++                  "
     + "       =$@@&)+++++++++++'##-.................=&++               "
     + "      .$$@-&)+++++++++;%#+$.....................=)+             "
     + "      $$,@-)+++++++++;%;@=........................,+            "
     + "     .$$@@-++++++++)-)@=............................            "
     + "     $,@---)++++&)@===............................,.            "
     + "    $-@---&)))-$$=..............................=)!.            "
     + "     --&-&&,,$=,==...........................=&+++!.            "
     + "      =,=$..=$+)+++++&@$=.............=$@&+++++!++!.            "
     + "           .)-++-+++++++++++++++++++++++++++!++!++!.            "
     + "           .+-++-+++++++++++++++++++++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!+++!!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            "
     + "           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            "
     + "            $.++-+++-+++++++++!++++!++++!+++!++!+.$             "
     + "              =.++++++++++++++!++++!++++!+++!++.=               "
     + "                 $..+++++++++++++++!++++++...$                  "
     + "                      $$==...........==$$                       "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                "
     + "                                                                ";

   Have_Drag : Boolean := False;

   Log : Gtk_Label;

   Drag_Icon            : Gdk_Pixbuf;
   Trashcan_Open        : Gdk_Pixbuf;
   Trashcan_Closed      : Gdk_Pixbuf;

   My_Target_String1  : constant Guint := 0;
   My_Target_String2  : constant Guint := 1;
   My_Target_Url      : constant Guint := 2;
   My_Target_Rootwin  : constant Guint := 3;

   package Widget_Callback is new
     Gtk.Handlers.Callback (Gtk.Widget.Gtk_Widget_Record);
   package Return_Callback is new
     Gtk.Handlers.Return_Callback (Gtk.Widget.Gtk_Widget_Record, Boolean);

   Target_Table : constant Target_Entry_Array
     := ((+"STRING",        0, My_Target_String1),
         (+"text/plain",    0, My_Target_String2),
         (+"text/uri-list", 0, My_Target_Url),
         (+"application/x-rootwin-drop", 0,
          My_Target_Rootwin));
   --  all the known data types in this application. Any MIME type can be used,
   --  as well a strings defined in the motif protocol, like "STRING".

   Target_Table_String : constant Target_Entry_Array
     := ((+"STRING",        0, My_Target_String1),
         (+"text/plain",    0, My_Target_String2));
   --  For a drop site that only accepts Data of type STRING or text/plain

   Target_Table_Url : constant Target_Entry_Array
     := (1 => (+"text/uri-list", 0, My_Target_Url));
   --  For a drop site that only accepts Data of type url.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates the drag-and-drop features of GtkAda."
        & " Several @bdrag-and-drop@B (dnd) protocols are supported, so that"
        & " your application can easily communicate with other external codes,"
        & " but this is mostly transparent for you."
        & ASCII.LF
        & "Although providing dnd capabilities in your application is not"
        & " difficult, it requires you to take care of several things, which"
        & " might be a little bit tricky the first time. You should look at"
        & " this demo while reading its source code, extensively documented"
        & " in testgtk/create_dnd.adb in the GtkAda distribution."
        & ASCII.LF
        & "Several things worth noting in this demo:"
        & ASCII.LF
        & ASCII.LF
        & " - Several @bdrag sources@B are available. The first only knows"
        & " how to transmitted plain text data. The second one only knows"
        & " about URLs, whereas the third one can provide any of the above."
        & ASCII.LF
        & ASCII.LF
        & " - Several @bdrop sites@B are also provided. Like the drag sources,"
        & " they all have special attributes and accept only special types"
        & " of data. Thus, some dnd operations will simply be rejected if"
        & " there is no common type between the drag source and the drop site."
        & ASCII.LF
        & ASCII.LF
        & " - In addition, all of the above have special types of @bactions@B"
        & " that they can handle. These are either @bAction_Copy@B,"
        & " @bAction_Move@B, or a combination of the two. These actions"
        & " control the default behavior of the drag and drop operation. The"
        & " action selected will be the first one common to both the drag"
        & " source and the drop site, unless you press @bshift@B at the same"
        & " time to force a move. Notice than when Action_Move is selected,"
        & " GtkAda asks the drag source to delete the data, thus the Delete"
        & " message that you see on the output."
        & ASCII.LF
        & ASCII.LF
        & " - The @btrashcan@B has a special behavior, since no default"
        & " behavior is associated to it, and everything is managed directly"
        & " by the demo, by connecting to the appropriate signals. We also"
        & " chose to change its visual aspect when the mouse is over the"
        & " widget and a drag-and-drop operation is taking place."
        & ASCII.LF
        & ASCII.LF
        & " - The @bicons@B can be freely modified. Note that the pixmap used"
        & " when dragging from the first button is different than the one used"
        & " for the other buttons. This can provide some interesting visual"
        & " clues for the user.";
   end Help;

   -------------
   -- Put_Log --
   -------------

   procedure Put_Log (Str : String) is
   begin
      Log.Set_Text (Str);
   end Put_Log;

   ----------------------
   -- Target_Drag_Drop --
   ----------------------
   --  A handler called for the signal "drag_drop", ie every time the user
   --  drops something on Widget.
   --  You need to provide such a handler when you do not provide any
   --  default behavior in Dest_Set, as is the case for the pixmap in Run.
   --
   --  Note that even if there is no common target between the drag source
   --  and the drop site, this handler is called anyway. However, since
   --  Get_Targets will return a Null_List, nothing will be printed.
   --
   --  This is the general form for handlers of "drag_drop".

   function Target_Drag_Drop
     (Widget : access Gtk_Widget_Record'Class;
      Args   : Glib.Values.GValues) return Boolean
   is
      Context : Drag_Context := Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint  := Get_Int (Nth (Args, 2));
      Y       : constant Gint  := Get_Int (Nth (Args, 3));
      Time    : constant Guint := Get_Uint (Nth (Args, 4));
      pragma Unreferenced (Context, X, Y, Time);
   begin
      Have_Drag := False;
      Put_Log ("Drop");
      Gtk_Image (Widget).Set (Trashcan_Closed);
      return False;
   end Target_Drag_Drop;

   -------------------------------
   -- Target_Drag_Data_Received --
   -------------------------------
   --  This function is called automatically every time some new data
   --  has been received on the trash can.
   --
   --  This is the general form of handlers for "drag_data_received".

   procedure Target_Drag_Data_Received
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Glib.Values.GValues)
   is
      Context : constant Drag_Context :=
         Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint  := Get_Int (Nth (Args, 2));
      Y       : constant Gint  := Get_Int (Nth (Args, 3));
      Data    : constant Gtk_Selection_Data :=
         From_Object (Get_Address (Nth (Args, 4)));

      Info : constant Guint := Get_Uint (Nth (Args, 5));
      --  third item of the Target_Entry

      Time : constant Guint := Get_Uint (Nth (Args, 6));

      pragma Unreferenced (Widget, X, Y, Info);
   begin
      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         Put_Log ("Received " & Get_Data_As_String (Data) & " in trashcan");
         Finish
           (Context, Success => True, Del => False, Time => Guint32 (Time));
      else
         Finish
           (Context, Success => False, Del => False, Time => Guint32 (Time));
      end if;
   end Target_Drag_Data_Received;

   ------------------------
   -- Target_Drag_Motion --
   ------------------------
   --  This is the handler for the signal "drag_motion".
   --  It is called every time the user is doing a drag-and-drop operation, and
   --  the mouse is currently over Widget (but not released yet).
   --  This is used to change the visual aspect of Widget to provide visual
   --  clues to the user.
   --  This is the general form for handlers of "drag_motion".

   function Target_Drag_Motion
      (Widget : access Gtk_Widget_Record'Class;
       Args   : Glib.Values.GValues)
      return Boolean
   is
      Context : constant Drag_Context :=
         Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint  := Get_Int (Nth (Args, 2));
      Y       : constant Gint  := Get_Int (Nth (Args, 3));
      Time    : constant Guint := Get_Uint (Nth (Args, 4));
      pragma Unreferenced (X, Y);
   begin
      if not Have_Drag then
         Have_Drag := True;
         Gtk_Image (Widget).Set (Trashcan_Open);
      end if;

      Drag_Status (Context, Get_Suggested_Action (Context), Guint32 (Time));

      return True;
   end Target_Drag_Motion;

   -----------------------
   -- Target_Drag_Leave --
   -----------------------
   --  A handler called whenever a drag-and-drop operation is being performed,
   --  and the mouse has just left the area covered by Widget on the screen.
   --  This is used to restore the default visual aspect of Widget.
   --
   --  Leave is also called when the drop is done on Widget.
   --
   --  This is the general form of handlers for "drag_leave".

   procedure Target_Drag_Leave
      (Widget : access Gtk_Widget_Record'Class;
       Args   : Glib.Values.GValues)
   is
      Context : Drag_Context := Drag_Context (Get_Object (Nth (Args, 1)));
      Time    : constant Guint := Get_Uint (Nth (Args, 2));
      pragma Unreferenced (Context, Time);
   begin
      Put_Log ("Leave");
      Have_Drag := False;
      Gtk_Image (Widget).Set (Trashcan_Closed);
   end Target_Drag_Leave;

   ------------------------------
   -- Label_Drag_Data_Received --
   ------------------------------
   --  This function is called automatically every time some new data
   --  has been received on the label "Drop Here".
   --
   --  This is the general form of handlers for "drag_data_received".

   procedure Label_Drag_Data_Received
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Glib.Values.GValues)
   is
      Context : constant Drag_Context :=
         Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint  := Get_Int (Nth (Args, 2));
      Y       : constant Gint  := Get_Int (Nth (Args, 3));
      Data    : constant Gtk_Selection_Data :=
         From_Object (Get_Address (Nth (Args, 4)));
      Info : constant Guint := Get_Uint (Nth (Args, 5));
      Time : constant Guint := Get_Uint (Nth (Args, 6));
      pragma Unreferenced (Widget, X, Y, Info);
   begin
      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         Put_Log ("Received "
                  & Get_Data_As_String (Data)
                  & " in label");
         Gtk.Dnd.Finish
           (Context, Success => True, Del => False, Time => Guint32 (Time));
      else
         Gtk.Dnd.Finish
           (Context, Success => False, Del => False, Time => Guint32 (Time));
      end if;
   end Label_Drag_Data_Received;

   --------------------------
   -- Source_Drag_Data_Get --
   --------------------------
   --  This is called when the user has dropped the item, and GtkAda needs
   --  to know what was actually dragged. The source widget should thus give
   --  some data, that will be transmitted to the drop widget through the
   --  signal "drag_data_received".
   --
   --  This is the general form of handlers for "drag_data_get".

   procedure Source_Drag_Data_Get
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Glib.Values.GValues)
   is
      Context : constant Drag_Context :=
         Drag_Context (Get_Object (Nth (Args, 1)));
      Data    : constant Gtk_Selection_Data :=
         From_Object (Get_Address (Nth (Args, 2)));
      Info : constant Guint := Get_Uint (Nth (Args, 3));
      Time : constant Guint := Get_Uint (Nth (Args, 4));
      pragma Unreferenced (Widget, Context, Time);
   begin
      if Info = My_Target_Rootwin then
         Put_Log ("I was dropped on the root window");

      elsif Info = My_Target_Url then
         Selection_Data_Set (Data, Get_Target (Data), 8,
                             "file://www.act-europe.fr");

      else
         Selection_Data_Set (Data, Get_Target (Data), 8,
                             "I'm Data!, Info was " & Guint'Image (Info));
      end if;
   end Source_Drag_Data_Get;

   -----------------------------
   -- Source_Drag_Data_Delete --
   -----------------------------
   --  This handler is called whenever the drop site of a drag-and-drop
   --  operation has decided that the data should be deleted, or if the
   --  selected action was Action_Move.
   --
   --  This is the general handler type for "drag_data_delete".

   procedure Source_Drag_Data_Delete
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args    : Glib.Values.GValues)
   is
      Context : constant Drag_Context :=
         Drag_Context (Get_Object (Nth (Args, 1)));
      pragma Unreferenced (Context, Widget);
   begin
      Put_Log ("Delete the data!");
   end Source_Drag_Data_Delete;

   -----------------
   -- Drag_Failed --
   -----------------
   --  Called in reaction to the "drag-failed" event.

   function Drag_Failed
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args    : Glib.Values.GValues) return Boolean is
   begin
      Put_Log ("Drag_Failed");
      --  Returning True here means canceling the animation where the
      --  drag icon goes back to the originating widget.
      return True;
   end Drag_Failed;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Table     : Gtk_Grid;
      Label     : Gtk_Label;
      Button    : Gtk_Button;
      Pixmap    : Gtk_Image;
      Box       : Gtk_Box;
   begin
      Set_Label (Frame, "Drag-and-Drop");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Table);
      Box.Pack_Start (Table);

      Drag_Icon := Gdk_New_From_Xpm_Data (Drag_Icon_Xpm);
      Trashcan_Open := Gdk_New_From_Xpm_Data (Trashcan_Open_Xpm);
      Trashcan_Closed := Gdk_New_From_Xpm_Data (Trashcan_Closed_Xpm);

      -----------------
      --  Drop sites --
      -----------------

      --  First drop site: a simple label

      Gtk_New (Label, "Drop plain text here." & ASCII.LF
               & "Action_Move only" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table_String, -- only STRING or text/plain
                        Action_Move);
      Table.Attach (Label, 0, 0);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      --  Second drop site: a simple label

      Gtk_New (Label, "Drop Url here." & ASCII.LF
               & "Action_Copy or Action_Move" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table_Url, -- only urls
                        Action_Copy + Action_Move);
      Table.Attach (Label, 1, 0);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      --  Third drop site: a simple label

      Gtk_New (Label, "Drop Anything" & ASCII.LF
               & "only Here" & ASCII.LF);
      Gtk.Dnd.Dest_Set (Label,
                        Dest_Default_All,
                        Target_Table, -- only urls
                        Action_Copy + Action_Move);
      Table.Attach (Label, 2, 0);

      Widget_Callback.Connect (Label, "drag_data_received",
                               Label_Drag_Data_Received'Access);

      -----------------
      --  Drag sites --
      -----------------

      --  First Drag site

      Gtk_New (Button, "Drag String from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table_String,
                          Action_Copy + Action_Move);
      Table.Attach (Button, 0, 1);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);
      Return_Callback.Connect (Button, "drag-failed",
                               Drag_Failed'Access);

      Gtk.Dnd.Source_Set_Icon_Pixbuf (Button, Drag_Icon);

      --  Second Drag site

      Gtk_New (Button, "Drag Url from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table_Url,
                          Action_Copy + Action_Move);
      Table.Attach (Button, 1, 1);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);

      --  Third Drag site

      Gtk_New (Button, "Drag String or" & ASCII.LF
               & "Url from Here" & ASCII.LF);
      Gtk.Dnd.Source_Set (Button,
                          Button1_Mask or Button3_Mask,
                          Target_Table,
                          Action_Copy + Action_Move + Action_Ask);
      Table.Attach (Button, 2, 1);

      Widget_Callback.Connect (Button, "drag_data_get",
                               Source_Drag_Data_Get'Access);
      Widget_Callback.Connect (Button, "drag_data_delete",
                               Source_Drag_Data_Delete'Access);

      --  Special drop site

      Gtk_New (Pixmap, Trashcan_Closed);
      Gtk.Dnd.Dest_Set (Pixmap);
      Table.Attach (Pixmap, 0, 2);

      Return_Callback.Connect (Pixmap, "drag_drop",
                               Target_Drag_Drop'Access);
      Widget_Callback.Connect (Pixmap, "drag_data_received",
                               Target_Drag_Data_Received'Access);
      Return_Callback.Connect (Pixmap, "drag_motion",
                               Target_Drag_Motion'Access);
      Widget_Callback.Connect (Pixmap, "drag_leave",
                               Target_Drag_Leave'Access);

      --  The log window
      Gtk_New (Log);
      Pack_Start (Box, Log);

      Show_All (Frame);
   end Run;

end Create_Dnd;

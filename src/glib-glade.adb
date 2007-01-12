-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--   Copyright (C) 1999-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
--                                                                   --
-- GATE is free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings; use Ada.Strings.Fixed;
with Glib.Object; use Glib.Object;

package body Glib.Glade is

   subtype Package_Range is Natural range 1 .. 1000;
   subtype Signal_Range is Natural range 1 .. 1000;

   Packages : array (Package_Range) of String_Ptr;
   Num_Packages : Natural := 0;
   --  Use by Add_Package and Gen_Packages to register all the packages
   --  referenced and print them

   type Instantiation_Rec is record
      Instantiation : String_Ptr;
      Rename        : String_Ptr;
   end record;
   --  If this instantiation is a rename, Rename is the name of the
   --  original signal instantiation to rename. It is null otherwise.

   Signal_Instantiations : array (Signal_Range) of Instantiation_Rec;
   Num_Signal_Instantiations : Natural := 0;
   --  Used by Add_Signal_Instantiation and Gen_Signal_Instantiations

   type Signal_Rec is record
      Widget : Node_Ptr;
      Handler, Signal, Class, Orig_Class : String_Ptr;
   end record;

   Signals : array (Signal_Range) of Signal_Rec;
   Num_Signals : Natural := 0;
   --  Used by Add_Signal and Gen_Signal_Instantiations

   function Add_Signal
     (Widget : Node_Ptr;
      Handler, Signal, Class, Orig_Class : String_Ptr) return String_Ptr;
   --  Add a specific handler of signal of a given class in Signals.
   --  If Handler is already present in the list of signals for the same
   --  top level widget, do not add it and return the class associated with
   --  the handler found, return null otherwise.
   --
   --  Widget is the node of the top level widget containing the signal.
   --  Orig_Class is the original class for the signal. If Orig_Class
   --  is different from Class, Class is the class of the widget passed
   --  to the callback using Object_Connect.

   procedure Add_Signal_Instantiation
     (S      : String_Ptr;
      Rename : String_Ptr := null);
   --  Add class S in Signal_Instantiation if not already present or if
   --  Rename isn't null.

   --------------
   -- Get_Part --
   --------------

   function Get_Part
     (S : String; Part : Positive; Separator : Character := ':') return String
   is
      Count : Natural := 0;
      First : Positive := S'First;

   begin
      for J in S'Range loop
         if S (J) = Separator then
            Count := Count + 1;

            if Count = Part then
               return S (First .. J - 1);
            else
               First := J + 1;
            end if;
         end if;
      end loop;

      if Count + 1 = Part then
         return S (First .. S'Last);
      else
         return "";
      end if;
   end Get_Part;

   -----------------
   -- Add_Package --
   -----------------

   procedure Add_Package (S : String) is
   begin
      for J in 1 .. Num_Packages loop
         if Packages (J).all = S then
            return;
         end if;
      end loop;

      Num_Packages := Num_Packages + 1;
      Packages (Num_Packages) := new String '(S);
   end Add_Package;

   ----------------
   -- Add_Signal --
   ----------------

   function Add_Signal
     (Widget : Node_Ptr;
      Handler, Signal, Class, Orig_Class : String_Ptr) return String_Ptr is
   begin
      for J in 1 .. Num_Signals loop
         if Signals (J).Widget = Widget
           and then Signals (J).Handler.all = Handler.all
         then
            return Signals (J).Class;
         end if;
      end loop;

      Num_Signals := Num_Signals + 1;
      Signals (Num_Signals).Widget     := Widget;
      Signals (Num_Signals).Handler    := Handler;
      Signals (Num_Signals).Signal     := Signal;
      Signals (Num_Signals).Class      := Class;
      Signals (Num_Signals).Orig_Class := Orig_Class;
      return null;
   end Add_Signal;

   ------------------------------
   -- Add_Signal_Instantiation --
   ------------------------------

   procedure Add_Signal_Instantiation
     (S      : String_Ptr;
      Rename : String_Ptr := null) is
   begin
      for J in 1 .. Num_Signal_Instantiations loop
         if Signal_Instantiations (J).Instantiation.all = S.all then
            --  Do not rename ourselves...

            if Rename = null or else Rename.all /= S.all then
               Signal_Instantiations (J).Rename := Rename;
            end if;

            return;
         end if;
      end loop;

      Num_Signal_Instantiations := Num_Signal_Instantiations + 1;
      Signal_Instantiations (Num_Signal_Instantiations) := (S, Rename);
   end Add_Signal_Instantiation;

   ------------
   -- Adjust --
   ------------

   function Adjust (S : String) return String is
      T : String (1 .. S'Length + 4096);
      K : Natural := 1;

   begin
      for J in S'Range loop
         if S (J) = ASCII.LF then
            T (K .. K + 15) := """ & ASCII.LF" & ASCII.LF & "& """;
            K := K + 16;

         --  Skip additional CR present on Win32

         elsif S (J) = '"' then
            T (K .. K + 1) := """""";
            K := K + 2;

         elsif S (J) /= ASCII.CR then
            T (K) := S (J);
            K := K + 1;
         end if;
      end loop;

      return T (1 .. K - 1);
   end Adjust;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent (N : Node_Ptr; Class : String) return Node_Ptr is
      P : Node_Ptr := N.Child;

   begin
      while P /= null loop
         if P.Tag.all = "class" then

            --  In some cases, Glade will have the child_name be the full name
            --  (e.g GtkDialog), whereas in some other cases it deletes the
            --  prefix (e.g Dialog). We have to handle both cases here

            if P.Value.all = Class then
               return P;

            --  Class can be shorter than the real class name

            elsif (P.Value'Length >= Class'Length + 3 and then
                P.Value (P.Value'First + 3 .. P.Value'First + 2 + Class'Length)
                  = Class)

            --  Take into account e.g Gtk[VH]Box

              or else P.Value (P.Value'First + 4 .. P.Value'Last) = Class
            then
               return P;
            else
               exit;
            end if;
         end if;

         P := P.Next;
      end loop;

      if N.Parent /= null then
         return Find_Parent (N.Parent, Class);
      else
         return null;
      end if;
   end Find_Parent;

   ---------------------
   -- Find_Top_Widget --
   ---------------------

   function Find_Top_Widget (N : Node_Ptr) return Node_Ptr is
      P, Q : Node_Ptr;
   begin
      Q := N;
      P := N.Parent;

      if P /= null then
         while P.Tag.all /= "glade-interface" loop
            Q := P;
            P := P.Parent;
         end loop;
      end if;

      return Q;
   end Find_Top_Widget;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child (N : Node_Ptr; Tag : String) return Node_Ptr is
      P    : Node_Ptr := N;
      Node : Node_Ptr;

   begin
      if N.Tag.all = Tag then
         return N;
      end if;

      while P /= null loop
         if P.Tag.all = Tag then
            return P;
         end if;

         if P.Child /= null then
            Node := Find_Child (P.Child, Tag);

            if Node /= null then
               return Node;
            end if;
         end if;

         P := P.Next;
      end loop;

      return null;
   end Find_Child;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (S : String; Separator : Character := '_') return String is
      First   : Positive;
      K       : Positive := 2;
      Last    : Integer := 0;
      R       : String (1 .. S'Length * 2);
      Has_Sep : Boolean;

      function Has_Separator (S : String) return Boolean;

      function Has_Separator (S : String) return Boolean is
      begin
         for J in S'Range loop
            if S (J) = Separator then
               return True;
            end if;
         end loop;

         return False;
      end Has_Separator;

   begin
      if S'Length = 0 then
         return S;
      end if;

      Has_Sep := Has_Separator (S);

      if S'Length > 4 and then
        (S (S'First .. S'First + 3) = "GTK_"
          or else S (S'First .. S'First + 3) = "GDK_")
      then
         First := S'First + 4;
      else
         First := S'First;
      end if;

      R (1) := To_Upper (S (First));

      for J in First + 1 .. S'Last loop
         --  Add a separator if the separator is not nul, if the current
         --  character is in upper case and if the string doesn't have any
         --  separator.

         if Separator /= ASCII.NUL and then Is_Upper (S (J))
           and then not Has_Sep and then J /= Last + 1
         then
            R (K) := Separator;
            K := K + 1;
            Last := J;
         end if;

         if S (J) = '-' then
            R (K) := Separator;
         else
            if R (K - 1) = Separator then
               R (K) := To_Upper (S (J));
            else
               R (K) := To_Lower (S (J));
            end if;
         end if;

         K := K + 1;
      end loop;

      return R (1 .. K - 1);
   end To_Ada;

   --------------
   -- To_Float --
   --------------

   function To_Float (S : String) return String is
      Exponentiated  : constant Boolean := Index (S, "e") /= 0;
      Mantissa_Start : Natural := 0;
   begin
      --  If the value is a power of 10 and >= 1_000_000 (which means it's
      --  being
      --  represented as "1e+06" or +07 or +08, etc.), insert a decimal point
      --  after the 1 to make it conform to Ada floating point syntax.
      --
      --  NOTE: This is _very_ Glade-specific, depending on exactly how Glade
      --  represents large (> 1_000_000) numbers.  When it uses exponential
      --  notation, it always normalizes the number, ie., makes it a single
      --  digit followed by a decimal point, and correspondingly adjusts the
      --  exponent.  However, powers of 10 are not provided with a decimal
      --  point, so their string representation will start off with "1e" or
      --  "-1e".  Meaning that if the value is exponentiated, and the "1e"
      --  substring starts in the first or second character, it must be a power
      --  of ten.
      --  If that string is not present (such as in 2.3e+09) or does not start
      --  at index S'First or S'First + 1 (as in 1.1e+06), then it is a
      --  valid number as is.

      if Exponentiated then
         Mantissa_Start := Index (S, "1e");
         if Mantissa_Start not in S'First .. S'First + 1 then
            --  This is not a power of ten.
            return S;
         else
            return Replace_Slice
              (S, Mantissa_Start, Mantissa_Start + 1, "1.0e");
         end if;

      else
         if Index (S, ".") /= 0 then
            return S;
         else
            return S & ".0";
         end if;
      end if;
   end To_Float;

   -------------
   -- Gen_Set --
   -------------

   procedure Gen_Set
     (N             : Node_Ptr;
      Name          : String;
      File          : File_Type;
      Prefix        : String  := "";
      Postfix       : String  := "";
      Is_Float      : Boolean := False;
      Property_Name : String := "")
   is
      P   : Node_Ptr;
      Cur : constant String := Get_Attribute (Find_Tag_With_Attribute
         (N, "widget", "id"), "id");
      Top : constant String := Get_Attribute (Find_Tag_With_Attribute
         (Find_Top_Widget (N), "widget", "id"), "id");

   begin
      if Property_Name = "" then
         P  := Find_Tag_With_Attribute (N.Child, "property", "name", Name);
      else
         P  := Find_Tag_With_Attribute
           (N.Child, "property", "name", Property_Name);
      end if;

      if P /= null then
         Put (File, "   Set_" & To_Ada (Name) & " (");

         if Top /= Cur then
            Put (File, To_Ada (Top) & ".");
         end if;

         Put (File, To_Ada (Cur) & ", ");

         if Prefix /= "" then
            if Prefix (Prefix'Last) = '"' then
               --  If the value is text, adjust it before inserting it.
               Put_Line (File, Prefix & Adjust (P.Value.all) & Postfix & ");");

            else
               Put_Line (File, Prefix & P.Value.all & Postfix & ");");
            end if;
         else
            if Is_Float then
               Put_Line (File, To_Float (P.Value.all) & ");");
            else
               Put_Line (File, To_Ada (P.Value.all) & ");");
            end if;
         end if;
      end if;
   end Gen_Set;

   procedure Gen_Set
     (N : Node_Ptr; Name, Field : String; File : File_Type)
   is
      P   : constant Node_Ptr := Find_Tag_With_Attribute
         (N.Child, "property", "name", Field);
      Cur : constant String := Get_Attribute (Find_Tag_With_Attribute
         (N, "widget", "id"), "id");
      Top : constant String := Get_Attribute (Find_Tag_With_Attribute
         (Find_Top_Widget (N), "widget", "id"), "id");

   begin
      if P /= null then
         Put (File, "   Set_" & To_Ada (Name) & " (");

         if Cur /= Top then
            Put (File, To_Ada (Top) & ".");
         end if;

         Put_Line (File, To_Ada (Cur) & ", " &
           To_Ada (P.Value.all) & ");");
      end if;
   end Gen_Set;

   procedure Gen_Set
     (N : Node_Ptr;
      Name, Field1, Field2, Field3, Field4 : String;
      File : File_Type;
      Is_Float : Boolean := False)
   is
      P   : constant Node_Ptr := Find_Tag_With_Attribute
         (N.Child, "property", "name", Field1);
      Q   : constant Node_Ptr := Find_Tag_With_Attribute
         (N.Child, "property", "name", Field2);
      R   : constant Node_Ptr := Find_Tag_With_Attribute
         (N.Child, "property", "name", Field3);
      S   : constant Node_Ptr := Find_Tag_With_Attribute
         (N.Child, "property", "name", Field4);
      Cur : constant String := Get_Attribute (Find_Tag_With_Attribute
         (N, "widget", "id"), "id");
      Top : constant String := Get_Attribute (Find_Tag_With_Attribute
         (Find_Top_Widget (N), "widget", "id"), "id");

   begin
      if P /= null then
         if Field2 = "" then
            if Is_Float then
               Put (File, "   Set_" & Name & " (");

               if Cur /= Top then
                  Put (File, To_Ada (Top) & ".");
               end if;

               Put (File, To_Ada (Cur) & ", " &
                 To_Float (P.Value.all));
            else
               Put (File, "   Set_" & Name & " (");

               if Cur /= Top then
                  Put (File, To_Ada (Top) & ".");
               end if;

               Put (File, To_Ada (Cur) & ", " &
                 To_Ada (P.Value.all));
            end if;

         elsif (Field3 = "" or else R /= null)
           and then (Field4 = "" or else S /= null)
         then
            Put (File, "   Set_" & Name & " (");

            if Cur /= Top then
               Put (File, To_Ada (Top) & ".");
            end if;

            Put (File, To_Ada (Cur) & ", ");

            if Is_Float then
               Put (File, To_Float (P.Value.all) & ", " & To_Float
                  (Q.Value.all));

            elsif Q /= null then
               Put (File, To_Ada (P.Value.all) & ", " & To_Ada
                  (Q.Value.all));
            else
               --  ??? Need to find a clean and uniform way of handling default
               --  integer values

               Put (File, To_Ada (P.Value.all) & ", -1");
            end if;

            if Field3 /= "" and R /= null then
               if Is_Float then
                  Put (File, ", " & To_Float (R.Value.all));
               else
                  Put (File, ", " & To_Ada (R.Value.all));
               end if;

               if Field4 /= "" and S /= null then
                  if Is_Float then
                     Put (File, ", " & To_Float (S.Value.all));
                  else
                     Put (File, ", " & To_Ada (S.Value.all));
                  end if;
               end if;
            end if;
         end if;

         Put_Line (File, ");");
      end if;
   end Gen_Set;

   -------------
   -- Gen_New --
   -------------

   procedure Gen_New
     (N        : Node_Ptr;
      Class    : String;
      Param1, Param2, New_Name : String := "";
      File     : File_Type;
      Prefix   : String := "";
      Postfix  : String := "")
   is
      P   : constant String := Get_Attribute (N, "id");
      Cur : constant String := Get_Attribute (N, "id");
      Top : constant String := Get_Attribute (Find_Top_Widget (N), "id");
   begin
      if N.Specific_Data.Created then
         return;
      end if;

      if P /= "" then
         Add_Package (Class);

         if Param1 /= "" then
            if Cur = Top then
               Put (File, "   Gtk." & Class & ".Initialize");
            else
               Put (File, "   Gtk_New");
            end if;

            if New_Name /= "" then
               Put (File, '_' & New_Name);
            end if;

            Put (File, " (");
            if Cur /= Top then
               Put (File, To_Ada (Top) & ".");
            end if;

            Put (File, To_Ada (P) & ", ");

            if Prefix /= "" then
               if Prefix (Prefix'Last) = '"' then
                  Put (File, Prefix & Adjust (Param1) & Postfix);

               else
                  Put (File, Prefix & Param1 & Postfix);
               end if;
            else
               Put (File, To_Ada (Param1));
            end if;

            if Param2 /= "" then
               Put (File, ", " & To_Ada (Param2));
            end if;

            Put_Line (File, ");");

         else
            if Cur = Top then
               Put (File, "   Gtk." & Class & ".Initialize");
            else
               Put (File, "   Gtk_New");
            end if;

            if New_Name /= "" then
               Put (File, '_' & New_Name);
            end if;

            Put (File, " (");

            if Cur /= Top then
               Put (File, To_Ada (Top) & ".");
            end if;

            Put_Line (File, To_Ada (P) & ");");
         end if;

         N.Specific_Data.Created := True;
      end if;
   end Gen_New;

   procedure Gen_New
     (N        : Node_Ptr;
      Class, Param1, Param2, Param3, Param4, Param5 : String;
      File     : File_Type;
      Prefix   : String := "";
      Postfix  : String := "")
   is
      Top : constant String := Get_Name (Find_Top_Widget (N));
      Cur : constant String := Get_Name (N);
   begin
      if N.Specific_Data.Created then
         return;
      end if;

      if Cur /= "" then
         Add_Package (Class);

         if Cur = Top then
            Put (File, "   Gtk." & Class & ".Initialize");
         else
            Put_Line (File, "   Gtk_New");
         end if;

         Put (File, "     (");

         if Cur /= Top then
            Put (File, To_Ada (Top) & ".");
         end if;

         Put (File, To_Ada (Cur) & ", ");

         if Prefix /= "" then
            Put (File, Prefix & Param1 & Postfix);
         else
            Put (File, To_Ada (Param1));
         end if;

         Put (File, ", " & To_Ada (Param2));
         Put_Line (File, ", " & To_Ada (Param3) & ", ");
         Put (File, "      " & To_Ada (Param4));

         if Param5 /= "" then
            Put (File, ", " & To_Ada (Param5));
         end if;

         Put_Line (File, ");");
         N.Specific_Data.Created := True;
      end if;
   end Gen_New;

   ---------------
   -- Gen_Child --
   ---------------

   procedure Gen_Child (N, Child : Node_Ptr; File : File_Type) is
      P      : constant String_Ptr := Get_Field (N, "name");
      Top    : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      Parent : String_Ptr;

   begin
      if P /= null then
         Parent := Get_Field
           (Find_Parent (N.Parent, Get_Part (Child.Value.all, 1)).Parent,
            "name");

         Add_Package (To_Package_Name (Get_Field (N, "class").all));
         Put (File, "   " & To_Ada (Top.all) & "." & To_Ada (P.all) &
              " := Get_" & To_Ada (Get_Part (Child.Value.all, 2)) & " (");

         if Parent /= Top then
            Put (File, To_Ada (Top.all) & ".");
         end if;

         Put_Line (File, To_Ada (Parent.all) & ");");
      end if;
   end Gen_Child;

   --------------------
   -- Gen_Call_Child --
   --------------------

   procedure Gen_Call_Child
     (N, Child, Parent : Node_Ptr;
      Class, Call : String;
      Param1, Param2, Param3 : String := "";
      File : File_Type)
   is
      pragma Unreferenced (Class);
      P      : constant String := Get_Attribute (N, "id");
      Top    : constant String := Get_Attribute (Find_Top_Widget (N), "id");

   begin
      if P /= "" then
         if Child = null then
            Put (File, "   " & Call & " (");

            declare
               Parent_Str : constant String := Get_Attribute (Parent, "id");
            begin

               if Top /= Parent_Str then
                  Put (File, To_Ada (Top) & ".");
               end if;

               Put (File, To_Ada (Parent_Str) & ", " &
                    To_Ada (Top) & "." & To_Ada (P));
            end;
         end if;

         if Param1 /= "" then
            Put (File, ", " & To_Ada (Get_Field (Child, Param1).all));
         end if;

         if Param2 /= "" then
            Put (File, ", " & To_Ada (Get_Field (Child, Param2).all));
         end if;

         if Param3 /= "" then
            Put (File, ", " & To_Ada (Get_Field (Child, Param3).all));
         end if;

         Put_Line (File, ");");
      end if;
   end Gen_Call_Child;

   ------------------
   -- Gen_Packages --
   ------------------

   procedure Gen_Packages (File : File_Type) is
      S : String_Ptr;

   begin
      for J in Package_Range'First .. Num_Packages loop
         S := Packages (J);

         --  If the package is fully qualified (contains a dot), print it
         --  as is. Otherwise, assume it is a child of Gtk.

         if Index (S.all, ".") /= 0 then
            Put_Line (File, "with " & S.all & "; use " & S.all & ";");
         else
            Put_Line (File, "with Gtk." & S.all & "; use Gtk." & S.all & ";");
         end if;
      end loop;
   end Gen_Packages;

   --------------------
   -- Reset_Packages --
   --------------------

   procedure Reset_Packages is
   begin
      Num_Packages := 0;
   end Reset_Packages;

   ----------------
   -- Reset_Tree --
   ----------------

   procedure Reset_Tree (N : Node_Ptr; Check_Next : Boolean := True) is
      M : Node_Ptr;
   begin
      N.Specific_Data.Created := False;
      N.Specific_Data.Initialized := False;
      N.Specific_Data.Has_Container := False;
      N.Specific_Data.Has_Accel_Group := False;
      N.Specific_Data.Has_Radio_Group := False;

      if Check_Next then
         M := N.Next;

         while M /= null loop
            Reset_Tree (M, Check_Next => False);
            M := M.Next;
         end loop;
      end if;

      if N.Child /= null then
         Reset_Tree (N.Child);
      end if;
   end Reset_Tree;

   ----------------
   -- Gen_Signal --
   ----------------

   procedure Gen_Signal
     (N            : Node_Ptr;
      File         : File_Type;
      Widget_Class : String := "";
      The_Object   : String := "")
   is
      P        : Node_Ptr := Find_Tag (N.Child, "signal");
      Top      : constant Node_Ptr := Find_Top_Widget (N);
      Current  : constant String := Get_Name (N);

      Returned : GType;
      Rename   : String_Ptr;
      Q        : Signal_Query;

      function Simple_Class (Class : String) return String;
      --  Return the simple name of a class.
      --  This currently simply replaces Gtk[HV]* by Gtk*

      function Simple_Class (Class : String) return String is
      begin
         if Widget_Class = "" then
            return Get_Class (N);
         end if;

         if Class /= "" and then Class'Length >= 5
           and then Class (Class'First .. Class'First + 2) = "Gtk"
           and then
             (Class (Class'First + 3) = 'H'
              or else Class (Class'First + 3) = 'V')
           and then Class (Class'First + 4) in 'A' .. 'Z'
           and then Class (Class'First + 4 .. Class'Last) /= "Button_Box"
         then
            return "Gtk" & Class (Class'First + 4 .. Class'Last);
         else
            return Class;
         end if;
      end Simple_Class;

      Orig_Class : constant String := Simple_Class (Widget_Class);
      Class      : String_Ptr;

      function Yes_No_To_True_False (S : in String) return String;
      --  Transform "yes" to "True" and "no" to "False".
      --  By default return "False".

      function Yes_No_To_True_False (S : in String) return String is
      begin
         if S = "yes" then
            return "True";
         else
            return "False";
         end if;
      end Yes_No_To_True_False;

   begin
      while P /= null loop
         declare
            Handler  : constant String := Get_Attribute (P, "handler");
            Name     : constant String := Get_Attribute (P, "name");
            Object   : constant String := Get_Attribute (P, "object");
            After    : constant String :=
              Yes_No_To_True_False (Get_Attribute (P, "after"));
            Id       : Signal_Id := Lookup (Type_From_Name (Orig_Class), Name);
            Connect_To_Buffer : Boolean := False;

         begin
            if Id = Null_Signal_Id
              and then Orig_Class = "GtkTextView"
            then
               Id := Lookup (Type_From_Name ("GtkTextBuffer"), Name);
               Connect_To_Buffer := True;
               Class := new String'("GtkTextBuffer");

            else
               Class := new String'(Orig_Class); --  ??? fix when having obj
            end if;

            if Id = Invalid_Signal_Id
              or else Id = Null_Signal_Id
            then
               Put_Line ("   -- Signal " & Name & " invalid for "
                         & To_Ada (Current));
               Put_Line ("   !!!");

            else
               Query (Id, Q);
               Returned := Return_Type (Q);

               if Object = "" then
                  Rename := Add_Signal
                    (Top,
                     new String'(Handler),
                     new String'(Name),
                     Class,
                     new String'(Orig_Class));
               else
                  Rename := Add_Signal
                    (Top,
                     new String'(Handler),
                     new String'(Name),
                     new String'("Gtk_Widget"),
                     new String'(Orig_Class));
               end if;

               if Returned <= GType_None and then Class.all /= "GtkWidget" then
                  Add_Signal_Instantiation (Class, Rename);
               end if;

               if Returned > GType_None then
                  Put (File, "   Return_Callback.");
               elsif Object /= "" then
                  Put (File, "   Widget_Callback.");
               else
                  Put
                    (File, "   " &
                     To_Ada
                       (Class (Class'First + 3 .. Class'Last)) & "_Callback.");
               end if;

               if Object /= "" then
                  Put (File, "Object_");
               end if;

               Put_Line (File, "Connect");
               Put (File, "     (");

               if The_Object = "" then
                  if Connect_To_Buffer then
                     Put (File, "Get_Buffer (");
                  end if;

                  if Top /= N then
                     Put (File, To_Ada (Get_Name (Top)) & ".");
                  end if;

                  Put (File, To_Ada (Current));

                  if Connect_To_Buffer then
                     Put (File, ")");
                  end if;
               else
                  Put (File, The_Object);
               end if;

               Put (File, ", """ & Name & """,");

               if Params (Q)'Length = 0 then
                  New_Line (File);
                  Put (File, "      ");
                  if Object /= "" then
                     Put (File, "Widget");
                  else
                     Put
                       (File, To_Ada (Class (Class'First + 3 .. Class'Last)));
                  end if;
                  Put (File, "_Callback.To_Marshaller (" & To_Ada (Handler)
                       & "'Access)");
               else
                  Put (File, " " & To_Ada (Handler) & "'Access");
               end if;

               if Object /= "" then
                  Put (File, ", " & To_Ada (Object));
               end if;

               if After /= "" then
                  Put (File, ", " & To_Ada (After));
               end if;

               Put_Line (File, ");");
            end if;
         end;

         P := Find_Tag (P.Next, "signal");
      end loop;
   end Gen_Signal;

   -------------------------------
   -- Gen_Signal_Instantiations --
   -------------------------------

   function Gen_Signal_Instantiations
     (Project : String; File : File_Type) return Natural
   is
      R, S     : String_Ptr;
      SR       : Signal_Rec;
      Prev_SR  : Signal_Rec;
      Kind     : String_Ptr;
      Returned : GType;
      Q        : Signal_Query;

   begin
      if Num_Signal_Instantiations > 0 then
         Put_Line (File, "with Gtk.Handlers;");
         Put_Line (File, "pragma Elaborate_All (Gtk.Handlers);");
      end if;

      for J in Signal_Range'First .. Num_Signal_Instantiations loop
         if Signal_Instantiations (J).Rename = null then
            S := new String'(To_Package_Name
              (Signal_Instantiations (J).Instantiation.all));
            Put_Line (File, "with " & S.all & "; use " & S.all & ";");
            Free (S);
         end if;
      end loop;

      New_Line (File);
      Put_Line (File, "package Callbacks_" & Project & " is");
      New_Line (File);

      for J in Signal_Range'First .. Num_Signal_Instantiations loop
         R := Signal_Instantiations (J).Rename;
         S := Signal_Instantiations (J).Instantiation;
         Put (File, "   package " &
           To_Ada (S (S'First + 3 .. S'Last)) & "_Callback ");

         if R = null then
            Put_Line (File, "is new");
            Put_Line (File, "     Gtk.Handlers.Callback (" &
              To_Ada (S.all) & "_Record);");

         else
            Put_Line (File, "renames " &
              To_Ada (R (R'First + 3 .. R'Last)) & "_Callback;");
         end if;

         New_Line (File);
      end loop;

      Put_Line (File, "end Callbacks_" & Project & ";");

      for J in Signal_Range'First .. Num_Signals loop
         SR := Signals (J);

         if Prev_SR.Widget = null or else SR.Widget /= Prev_SR.Widget then
            Prev_SR := SR;
            Put_Line (File, "with Gtk.Arguments;");
            Put_Line (File, "with Gtk.Widget; use Gtk.Widget;");
            New_Line (File);
            Put_Line (File, "package " &
              To_Ada (Get_Name (SR.Widget)) &
              "_Pkg.Callbacks is");

            for J in Signal_Range'First .. Num_Signals loop
               if Signals (J).Widget.all = SR.Widget.all then
                  Query (Lookup (Type_From_Name (Signals (J).Orig_Class.all),
                                 Signals (J).Signal.all),
                         Q);
                  Returned := Return_Type (Q);

                  if Returned > GType_None then
                     Put (File, "   function ");
                  else
                     Put (File, "   procedure ");
                  end if;

                  Put_Line (File, To_Ada (Signals (J).Handler.all));
                  Put (File, "     (Object : access ");

                  if Returned > GType_None then
                     Put (File, "Gtk_Widget");
                  else
                     Put (File, To_Ada (Signals (J).Class.all));
                  end if;

                  Put (File, "_Record'Class");

                  if Params (Q)'Length > 0 then
                     Put_Line (File, ";");
                     Put (File, "      Params : Gtk.Arguments.Gtk_Args)");
                  else
                     Put (File, ")");
                  end if;

                  if Returned > GType_None then
                     Put (File, " return Boolean");
                  end if;

                  Put_Line (File, ";");
                  New_Line (File);
               end if;
            end loop;

            Put_Line (File, "end " &
              To_Ada (Get_Name (SR.Widget)) & "_Pkg.Callbacks;");

            --  ??? Need to find a way to put only the required packages

            Put_Line (File, "with System; use System;");
            Put_Line (File, "with Glib; use Glib;");
            Put_Line (File, "with Gdk.Event; use Gdk.Event;");
            Put_Line (File, "with Gdk.Types; use Gdk.Types;");
            Put_Line (File, "with Gtk.Accel_Group; use Gtk.Accel_Group;");
            Put_Line (File, "with Gtk.Object; use Gtk.Object;");
            Put_Line (File, "with Gtk.Enums; use Gtk.Enums;");
            Put_Line (File, "with Gtk.Style; use Gtk.Style;");
            Put_Line (File, "with Gtk.Widget; use Gtk.Widget;");
            New_Line (File);
            Put_Line (File, "package body " &
              To_Ada (Get_Name (SR.Widget)) &
              "_Pkg.Callbacks is");
            New_Line (File);
            Put_Line (File, "   use Gtk.Arguments;");
            New_Line (File);

            for J in Signal_Range'First .. Num_Signals loop
               if Signals (J).Widget.all = SR.Widget.all then
                  declare
                     Handler : constant String :=
                       To_Ada (Signals (J).Handler.all);
                     Dashes : constant String (1 .. Handler'Length + 6) :=
                       (others => '-');

                  begin
                     Query (Lookup
                            (Type_From_Name (Signals (J).Orig_Class.all),
                             Signals (J).Signal.all),
                            Q);
                     Returned := Return_Type (Q);

                     Put_Line (File, "   " & Dashes);
                     Put_Line (File, "   -- " & Handler & " --");
                     Put_Line (File, "   " & Dashes);
                     New_Line (File);

                     if Returned > GType_None then
                        Put (File, "   function ");
                     else
                        Put (File, "   procedure ");
                     end if;

                     Put_Line (File, Handler);
                     Put (File, "     (Object : access ");

                     if Returned > GType_None then
                        Put (File, "Gtk_Widget");
                     else
                        Put (File, To_Ada (Signals (J).Class.all));
                     end if;

                     Put (File, "_Record'Class");

                     declare
                        P : constant GType_Array := Params (Q);
                     begin

                        if P'Length > 0 then
                           Put_Line (File, ";");
                           Put (File,
                                "      Params : Gtk.Arguments.Gtk_Args)");
                        else
                           Put (File, ")");
                        end if;

                        if Returned > GType_None then
                           Put (File, " return Boolean");
                        end if;

                        New_Line (File);
                        Put_Line (File, "   is");

                        for K in P'Range loop
                           Kind := new String'(To_Ada (Type_Name (P (K))));

                           if Kind.all = "Gpointer" then
                              Free (Kind);
                              Kind := new String'("Address");

                           elsif Kind.all = "Gtk_String" then
                              Free (Kind);
                              Kind := new String'("String");
                           end if;

                           if Kind.all = "Gchararray" then
                              Put (File, "      Arg" &
                                   Trim (Guint'Image (K - P'First + 1), Left) &
                                   " : UTF8_String := ");
                           else
                              Put (File, "      Arg" &
                                   Trim (Guint'Image (K - P'First + 1), Left) &
                                   " : " & Kind.all & " := ");
                           end if;

                           --  ??? This whole section is ugly. Need to find a
                           --  cleaner and more automated way of generating the
                           --  right code.

                           if Kind.all = "Gdk_Event" then
                              Put (File, "To_Event");
                           elsif Kind (Kind'First .. Kind'First + 2)
                             = "Gtk"
                           then
                              if Kind.all = "Gtk_Clist_Row"
                                or else Kind.all = "Gtk_Ctree_Node"
                                or else Kind.all = "Gtk_Accel_Group"
                                or else Kind.all = "Gtk_Style"
                              then
                                 Put (File, Kind.all & " (To_C_Proxy");
                              elsif Kind.all = "Gtk_Object"
                                or else Kind.all = "Gtk_Widget"
                              then
                                 Put (File, Kind.all & " (To_Object");
                              else
                                 Put (File, Kind.all & "'Val (To_Gint");
                              end if;

                           elsif Kind (Kind'First .. Kind'First + 2)
                             = "Gdk"
                           then
                              Put (File, Kind.all & " (To_Gint");
                           elsif Kind.all = "Gchararray" then
                              Put (File, "To_String");
                           else
                              Put (File, "To_" & Kind.all);
                           end if;

                           Put (File, " (Params, " &
                                Trim (Guint'Image (K - P'First + 1), Left));

                           if Kind (Kind'First .. Kind'First + 2) = "Gtk"
                             or else Kind (Kind'First .. Kind'First + 2) =
                             "Gdk"
                           then
                              if Kind.all = "Gdk_Event" then
                                 Put_Line (File, ");");
                              else
                                 Put_Line (File, "));");
                              end if;
                           else
                              Put_Line (File, ");");
                           end if;

                           Free (Kind);
                        end loop;
                     end;

                     Put_Line (File, "   begin");

                     if Returned > GType_None then
                        Put_Line (File, "      return False;");
                     else
                        Put_Line (File, "      null;");
                     end if;

                     Put_Line (File, "   end " & Handler & ";");
                     New_Line (File);
                  end;
               end if;
            end loop;

            Put_Line (File, "end " &
              To_Ada (Get_Name (SR.Widget)) &
              "_Pkg.Callbacks;");
         end if;
      end loop;

      return Num_Signal_Instantiations;
   end Gen_Signal_Instantiations;

   ---------------------
   -- Gettext_Support --
   ---------------------

   function Gettext_Support (N : Node_Ptr) return Boolean is
      P : Node_Ptr;
   begin
      if not Gettext then
         return False;
      end if;

      if N /= null then
         P := Find_Tag_With_Attribute (N.Child, "property",
               "translatable");

         declare
            S : constant String := Get_Attribute (P, "translatable");
         begin
            return S = "yes";
         end;
      end if;

      return False;
   end Gettext_Support;

   ---------------------
   -- To_Package_Name --
   ---------------------

   --  The following table is needed to transform Gtk+ names that cannot
   --  be mapped automatically (e.g Ada reserved words) into the correct GtkAda
   --  package name.

   type Package_Mapping is record
      Gtk_Name, GtkAda_Package : String_Ptr;
   end record;

   type Special_Packages_Type is array (Positive range <>) of Package_Mapping;

   Special_Packages : constant Special_Packages_Type :=
     ((new String '("GtkRange"),      new String '("Gtk.GRange")),
      (new String '("GtkEntry"),      new String '("Gtk.GEntry")),
      (new String '("GtkCTree"),      new String '("Gtk.Ctree")),
      (new String '("GtkCList"),      new String '("Gtk.Clist")),
      (new String '("GtkHScale"),     new String '("Gtk.Scale")),
      (new String '("GtkVScale"),     new String '("Gtk.Scale")),
      (new String '("GtkHBox"),       new String '("Gtk.Box")),
      (new String '("GtkVBox"),       new String '("Gtk.Box")),
      (new String '("GtkHPaned"),     new String '("Gtk.Paned")),
      (new String '("GtkVPaned"),     new String '("Gtk.Paned")),
      (new String '("GtkHRuler"),     new String '("Gtk.Ruler")),
      (new String '("GtkVRuler"),     new String '("Gtk.Ruler")),
      (new String '("GtkHScrollbar"), new String '("Gtk.Scrollbar")),
      (new String '("GtkVScrollbar"), new String '("Gtk.Scrollbar")),
      (new String '("GtkHSeparator"), new String '("Gtk.Separator")),
      (new String '("GtkVSeparator"), new String '("Gtk.Separator")));

   function To_Package_Name (S : String) return String is
   begin
      for J in Special_Packages'Range loop
         if S = Special_Packages (J).Gtk_Name.all then
            return Special_Packages (J).GtkAda_Package.all;
         end if;
      end loop;

      --  No fix up needed

      return "Gtk." & To_Ada (S (S'First + 3 .. S'Last));
   end To_Package_Name;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (N        : Node_Ptr;
      Property : String) return String_Ptr
   is
      C : Node_Ptr;
   begin
      if N = null then
         return null;
      end if;

      C := N.Child;

      while C /= null loop
         if C.Tag.all = "property"
           and then Get_Attribute (C, "name") = Property
         then
            return C.Value;
         end if;

         C := C.Next;
      end loop;

      return null;
   end Get_Property;

   function Get_Property
     (N        : Node_Ptr;
      Property : String;
      Default  : String := "") return String
   is
      P : constant String_Ptr := Get_Property (N, Property);
   begin
      if P = null then
         return Default;
      else
         return P.all;
      end if;
   end Get_Property;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (N : Node_Ptr) return String is
   begin
      if N = null then
         return "";
      end if;

      if N.Tag.all = "widget" then
         return Get_Attribute (N, "class");
      end if;

      return "";
   end Get_Class;

   ---------------
   -- Get_Name  --
   ---------------

   function Get_Name (N : Node_Ptr) return String is
   begin
      if N = null then
         return "";
      end if;

      return Get_Attribute (N, "id");
   end Get_Name;

end Glib.Glade;

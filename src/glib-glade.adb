-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                      Copyright (C) 1999                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Glib.Glade is

   subtype Package_Range is Natural range 1 .. 100;
   subtype Signal_Range is Natural range 1 .. 100;

   Packages : array (Package_Range) of String_Ptr;
   Num_Packages : Natural := 0;
   --  Use by Add_Package and Gen_Packages to register all the packages
   --  referenced and print them

   Signal_Instanciations : array (Signal_Range) of String_Ptr;
   Num_Signal_Instanciations : Natural := 0;
   --  Used by Add_Signal_Instanciation and Gen_Signal_Instanciations

   type Signal_Rec is record
      Signal, Class : String_Ptr;
   end record;

   Signals : array (Signal_Range) of Signal_Rec;
   Num_Signals : Natural := 0;
   --  Used by Add_Signal and Gen_Signal_Instanciations

   procedure Add_Package (S : String);
   --  Add package S in Packages if S isn't already present

   procedure Add_Signal (Signal, Class : String_Ptr);
   --  Add widget S in Signals if S isn't already present

   procedure Add_Signal_Instanciation (S : String_Ptr);
   --  Add widget S in Signal_Instanciation if S isn't already present

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

   procedure Add_Signal (Signal, Class : String_Ptr) is
   begin
      Num_Signals := Num_Signals + 1;
      Signals (Num_Signals).Signal := Signal;
      Signals (Num_Signals).Class := Class;
   end Add_Signal;

   ------------------------------
   -- Add_Signal_Instanciation --
   ------------------------------

   procedure Add_Signal_Instanciation (S : String_Ptr) is
   begin
      for J in 1 .. Num_Signal_Instanciations loop
         if Signal_Instanciations (J).all = S.all then
            return;
         end if;
      end loop;

      Num_Signal_Instanciations := Num_Signal_Instanciations + 1;
      Signal_Instanciations (Num_Signal_Instanciations) := S;
   end Add_Signal_Instanciation;

   -----------------
   -- Find_Parent --
   -----------------

   function Find_Parent (N : Node_Ptr; Class : String) return Node_Ptr is
      P : Node_Ptr := N.Child;

   begin
      while P /= null loop
         if P.Tag.all = "class" then
            if P.Value (P.Value'First + 3 .. P.Value'Last) = Class

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
      Has_Sep := Has_Separator (S);

      if S'Length > 4 and then S (S'First .. S'First + 3) = "GTK_" then
         First := S'First + 4;
      else
         First := S'First;
      end if;

      R (1) := To_Upper (S (First));

      for J in First + 1 .. S'Last loop
         --  Add a separator if the separator is not nul, if the current
         --  character is in upper case and if the string doesn't have any
         --  separator.

         if Separator /= ASCII.Nul and then Is_Upper (S (J))
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

   -------------
   -- Gen_Set --
   -------------

   procedure Gen_Set
     (N : Node_Ptr; Class, Name : String;
      File : File_Type; Delim : Character := ' ')
   is
      P : Node_Ptr := Find_Tag (N.Child, Name);

   begin
      if P /= null then
         Add_Package (Class);
         Put (File, "   " & Class & ".Set_" & To_Ada (Name) & " (Gtk_" &
           Class & " (" & To_Ada (Find_Tag (N.Child, "name").Value.all) &
           "), ");

         if Delim /= ' ' then
            Put_Line (File, Delim & P.Value.all & Delim & ");");
         else
            Put_Line (File, To_Ada (P.Value.all) & ");");
         end if;
      end if;
   end Gen_Set;

   procedure Gen_Set
     (N : Node_Ptr;
      Class, Name, Field1, Field2, Field3 : String;
      File : File_Type)
   is
      P : Node_Ptr := Find_Tag (N.Child, Field1);
      Q : Node_Ptr := Find_Tag (N.Child, Field2);
      R : Node_Ptr;

   begin
      if Field3 /= "" then
         R := Find_Tag (N.Child, Field3);
      end if;

      if P /= null and then Q /= null
        and then (Field3 = "" or else R /= null)
      then
         Add_Package (Class);
         Put (File, "   " & Class & ".Set_" & Name & " (Gtk_" & Class & " (" &
           To_Ada (Find_Tag (N.Child, "name").Value.all) & "), " &
           To_Ada (P.Value.all) & ", " & To_Ada (Q.Value.all));

         if R /= null then
            Put_Line (File, ", " & To_Ada (R.Value.all) & ");");
         else
            Put_Line (File, ");");
         end if;
      end if;
   end Gen_Set;

   -------------
   -- Gen_New --
   -------------

   procedure Gen_New
     (N : Node_Ptr;
      Class, Param1, Param2, New_Name : String := "";
      File : File_Type; Delim : Character := ' ')
   is
      P : Node_Ptr;

   begin
      if N.Specific_Data.Created then
         return;
      end if;

      P := Find_Tag (N.Child, "name");

      if P /= null then
         Add_Package (Class);

         if Param1 /= "" then
            Put (File, "   " & Class & ".Gtk_New");

            if New_Name /= "" then
               Put (File, '_' & New_Name);
            end if;

            if Delim /= ' ' then
               Put (File, " (" & To_Ada (P.Value.all) & ", " & Delim &
                 Param1 & Delim);
            else
               Put
                 (File, " (" & To_Ada (P.Value.all) & ", " & To_Ada (Param1));
            end if;

            if Param2 /= "" then
               Put (File, ", " & To_Ada (Param2));
            end if;

            Put_Line (File, ");");

         else
            Put_Line (File, "   " & Class & ".Gtk_New (" &
              To_Ada (P.Value.all) & ");");
         end if;

         N.Specific_Data.Created := True;
      end if;
   end Gen_New;

   ---------------
   -- Gen_Child --
   ---------------

   procedure Gen_Child (N, Child : Node_Ptr; File : File_Type) is
      P : Node_Ptr := Find_Tag (N.Child, "name");
   begin
      if P /= null then
         Put_Line
           (File, "   " & To_Ada (P.Value.all) & " := Get_" &
            To_Ada (Get_Part (Child.Value.all, 2)) & " (" &
            To_Ada (Find_Tag (Find_Parent
              (N.Parent, Get_Part (Child.Value.all, 1)),
              "name").Value.all) & ");");
      end if;
   end Gen_Child;

   --------------------
   -- Gen_Call_Child --
   --------------------

   procedure Gen_Call_Child (N, Child : Node_Ptr;
     Class, Call : String;
     Param1, Param2, Param3 : String := "";
     File : File_Type)
   is
      P : String_Ptr := Get_Field (N, "name");

   begin
      if P /= null then
         Add_Package (Class);

         if Child = null then
            Put (File, "   " & Class & '.' & Call & " (Gtk_" & Class & " (" &
              To_Ada (Get_Field (N.Parent, "name").all) & "), " &
              To_Ada (P.all));

         else
            Put (File, "   " & Class & '.' & Call & " (" &
              To_Ada
                (Find_Tag (Find_Parent (N.Parent, Class), "name").Value.all) &
              ", " & To_Ada (P.all));
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

   -----------------
   -- Gen_Package --
   -----------------

   procedure Gen_Packages (File : File_Type) is
   begin
      for J in Package_Range'First .. Num_Packages loop
         Put_Line (File, "with Gtk." & Packages (J).all & "; use Gtk." &
           Packages (J).all & ";");
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
   -- Gen_Signal --
   ----------------

   procedure Gen_Signal (N : Node_Ptr; File : File_Type) is
      P       : Node_Ptr := Find_Tag (N.Child, "signal");
      Class   : String_Ptr;
      Handler : String_Ptr;

   begin
      while P /= null loop
         Handler := Get_Field (P, "handler");
         Class := Get_Field (N, "class");
         Add_Signal_Instanciation (Class);
         Add_Signal (Handler, Class);
         Put_Line (File, "   Cb_Id := " &
           Class (Class'First + 3 .. Class'Last) & "_Callback.Connect");
         Put_Line (File, "     (" & To_Ada (Get_Field (N, "name").all) &
           ", """ & Get_Field (P, "name").all & """, " & To_Ada (Handler.all) &
           "'Access);");
         P := Find_Tag (P.Next, "signal");
      end loop;
   end Gen_Signal;

   -------------------------------
   -- Gen_Signal_Instanciations --
   -------------------------------

   function Gen_Signal_Instanciations (File : File_Type) return Natural is
   begin
      if Num_Signal_Instanciations > 0 then
         Put_Line (File, "with Gtk.Signal;");

         for J in Signal_Range'First .. Num_Signal_Instanciations loop
            Put_Line (File, "with " &
              To_Ada (Signal_Instanciations (J).all, '.') & "; use " &
              To_Ada (Signal_Instanciations (J).all, '.') & ";");
         end loop;

         New_Line (File);
         Put_Line (File, "package Callbacks is");
         New_Line (File);

         for J in Signal_Range'First .. Num_Signal_Instanciations loop
            Put_Line (File, "   package " &
              Signal_Instanciations (J)
                (Signal_Instanciations (J)'First + 3 ..
                 Signal_Instanciations (J)'Last) & "_Callback is new");
            Put_Line (File, "     Gtk.Signal.Void_Callback (" &
              To_Ada (Signal_Instanciations (J).all) & ");");
            New_Line (File);
         end loop;

         for J in Signal_Range'First .. Num_Signals loop
            Put_Line (File, "   procedure " & To_Ada (Signals (J).Signal.all) &
              " (Object : in out " & To_Ada (Signals (J).Class.all) & ");");
            New_Line (File);
         end loop;

         Put_Line (File, "end Callbacks;");
         New_Line (File);
         Put_Line (File, "package body Callbacks is");
         New_Line (File);

         for J in Signal_Range'First .. Num_Signals loop
            Put_Line (File, "   procedure " & To_Ada (Signals (J).Signal.all) &
              " (Object : in out " & To_Ada (Signals (J).Class.all) & ") is");
            Put_Line (File, "   begin");
            Put_Line (File, "      null;");
            Put_Line (File, "   end " & To_Ada (Signals (J).Signal.all) & ";");
            New_Line (File);
         end loop;

         Put_Line (File, "end Callbacks;");
      end if;

      return Num_Signal_Instanciations;
   end Gen_Signal_Instanciations;

end Glib.Glade;

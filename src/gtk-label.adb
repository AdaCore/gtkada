with Interfaces.C.Strings;

package body Gtk.Label is

   package C renames Interfaces.C;

   -----------
   --  Get  --
   -----------

   function Get (Label : in Gtk_Label'Class) return String is
      procedure Internal (Label : in     System.Address;
                          Str   :    out C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_get");
      Temp : C.Strings.chars_ptr;
   begin
      Internal (Get_Object (Label), Temp);
      return C.Strings.Value (Temp);
   end Get;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String) is
      function Internal (Str : in String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");
   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Gtk_New;


   -----------
   --  Set  --
   -----------

   procedure Set (Label : in out Gtk_Label'Class;
                  Str   : in     String) is
      procedure Internal (Label : in System.Address;
                          Str   : in String);
      pragma Import (C, Internal, "gtk_label_set");
   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set;

   -------------------
   --  Set_Justify  --
   -------------------

   procedure Set_Justify (Label : in out Gtk_Label'Class;
                          Jtype : in     Enums.Gtk_Justification) is
      procedure Internal (Label : in System.Address;
                          Jtype : in Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");
   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;


end Gtk.Label;

with Interfaces.C.Strings;

package body Gtk.Main is

   package C renames Interfaces.C;


   ------------------
   --  Set_Locale  --
   ------------------

   function Set_Locale return String is
      function Internal return C.Strings.Chars_Ptr;
      pragma Import (C, Internal, "gtk_set_locale");
   begin
      return C.Strings.Value (Internal);
   end Set_Locale;


   -------------------
   --  Set_Localed  --
   -------------------

   procedure Set_Locale is
      Dummy : constant String := Set_Locale;
   begin
      null;
   end Set_Locale;


end Gtk.Main;

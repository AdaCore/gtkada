with Interfaces.C;

package body Gdk.Font is

   package C renames Interfaces.C;


   -----------
   --  "="  --
   -----------

   function "=" (Fonta, Fontb : in Gdk_Font) return Boolean is
      function Internal (Fonta, Fontb : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_font_equal");
   begin
      return To_Boolean (Internal (Get_Object (Fonta),
                                   Get_Object (Fontb)));
   end "=";


   --------------------
   --  Char_Measure  --
   --------------------

   function Char_Measure (Font : in Gdk_Font;
                          Char : in Character) return Gint is
      function Internal (Font : in System.Address;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_measure");
   begin
      return Internal (Get_Object (Font), C.To_C (Char));
   end Char_Measure;


   ------------------
   --  Char_Width  --
   ------------------

   function Char_Width (Font : in Gdk_Font;
                        Char : in Character) return Gint is
      function Internal (Font : in System.Address;
                         Char : in C.char) return Gint;
      pragma Import (C, Internal, "gdk_char_width");
   begin
      return Internal (Get_Object (Font), C.To_C (Char));
   end Char_Width;


   --------------------
   --  Fontset_Load  --
   --------------------

   procedure Fontset_Load (Font         :   out Gdk_Font;
                           Fontset_Name : in    String) is
      function Internal (Fontset_Name : in String) return System.Address;
      pragma Import (C, Internal, "gdk_fontset_load");
   begin
      Set_Object (Font, Internal (Fontset_Name & ASCII.NUL));
   end Fontset_Load;


   ----------
   --  Id  --
   ----------

   function Id (Font : in Gdk_Font) return Gint is
      function Internal (Font : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_font_id");
   begin
      return Internal (Get_Object (Font));
   end Id;


   ------------
   --  Load  --
   ------------

   procedure Load (Font      :    out Gdk_Font;
                   Font_Name : in     String) is
      function Internal (Font_Name : in String) return System.Address;
      pragma Import (C, Internal, "gdk_font_load");
   begin
      Set_Object (Font, Internal (Font_Name & ASCII.NUL));
   end Load;


   ----------------------
   --  String_Measure  --
   ----------------------

   function String_Measure (Font : in Gdk_Font;
                            Str  : in String) return Gint is
      function Internal (Font : in System.Address;
                         Str  : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_measure");
   begin
      return Internal (Get_Object (Font), Str & ASCII.NUL);
   end String_Measure;


   --------------------
   --  String_Width  --
   --------------------

   function String_Width (Font : in Gdk_Font;
                          Str  : in String) return Gint is
      function Internal (Font : in System.Address;
                         Str : in String) return Gint;
      pragma Import (C, Internal, "gdk_string_width");
   begin
      return Internal (Get_Object (Font), Str & ASCII.NUL);
   end String_Width;


   --------------------
   --  Text_Measure  --
   --------------------

   function Text_Measure (Font : in Gdk_Font;
                          Text : in String) return Gint is
      function Internal (Font        : in System.Address;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_measure");
   begin
      return Internal (Get_Object (Font), Text, Text'Length);
   end Text_Measure;


   ------------------
   --  Text_Width  --
   ------------------

   function Text_Width (Font : in Gdk_Font;
                        Text : in String) return Gint is
      function Internal (Font        : in System.Address;
                         Text        : in String;
                         Text_Length : in Gint) return Gint;
      pragma Import (C, Internal, "gdk_text_width");
   begin
      return Internal (Get_Object (Font), Text, Text'Length);
   end Text_Width;

end Gdk.Font;

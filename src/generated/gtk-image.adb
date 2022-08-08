------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Image is

   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk_Icon_Size)
   is
      procedure Internal
        (Image : System.Address;
         Name  : out Gtkada.Types.Chars_Ptr;
         Size  : out Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_icon_name");
      Str : chars_ptr;
   begin
      Internal (Get_Object (Image), Str, Size);
      Name := new String'(Value (Str));
   end Get_Icon_Name;

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String
   is
      procedure Internal
        (Image    : System.Address;
         Stock_Id : out Gtkada.Types.Chars_Ptr;
         Size     : out Gint);
      pragma Import (C, Internal, "gtk_image_get_stock");

      Stock : Gtkada.Types.Chars_Ptr;
      Sze   : Gint;

   begin
      Internal (Get_Object (Image), Stock, Sze);
      Size.all := Gtk.Enums.Gtk_Icon_Size'Val (Sze);
      return Gtkada.Types.Value (Stock);
   end Get;

   package Type_Conversion_Gtk_Image is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Image);

   -------------------
   -- Gtk_Image_New --
   -------------------

   function Gtk_Image_New return Gtk_Image is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image);
      return Image;
   end Gtk_Image_New;

   ----------------------------------
   -- Gtk_Image_New_From_Animation --
   ----------------------------------

   function Gtk_Image_New_From_Animation
      (Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image, Animation);
      return Image;
   end Gtk_Image_New_From_Animation;

   -----------------------------
   -- Gtk_Image_New_From_File --
   -----------------------------

   function Gtk_Image_New_From_File
      (Filename : UTF8_String) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image, Filename);
      return Image;
   end Gtk_Image_New_From_File;

   ------------------------------
   -- Gtk_Image_New_From_Gicon --
   ------------------------------

   function Gtk_Image_New_From_Gicon
      (Icon : Glib.G_Icon.G_Icon;
       Size : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Gicon (Image, Icon, Size);
      return Image;
   end Gtk_Image_New_From_Gicon;

   ----------------------------------
   -- Gtk_Image_New_From_Icon_Name --
   ----------------------------------

   function Gtk_Image_New_From_Icon_Name
      (Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Icon_Name (Image, Icon_Name, Size);
      return Image;
   end Gtk_Image_New_From_Icon_Name;

   ---------------------------------
   -- Gtk_Image_New_From_Icon_Set --
   ---------------------------------

   function Gtk_Image_New_From_Icon_Set
      (Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image, Icon_Set, Size);
      return Image;
   end Gtk_Image_New_From_Icon_Set;

   -------------------------------
   -- Gtk_Image_New_From_Pixbuf --
   -------------------------------

   function Gtk_Image_New_From_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image, Pixbuf);
      return Image;
   end Gtk_Image_New_From_Pixbuf;

   ---------------------------------
   -- Gtk_Image_New_From_Resource --
   ---------------------------------

   function Gtk_Image_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Resource (Image, Resource_Path);
      return Image;
   end Gtk_Image_New_From_Resource;

   ------------------------------
   -- Gtk_Image_New_From_Stock --
   ------------------------------

   function Gtk_Image_New_From_Stock
      (Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize (Image, Stock_Id, Size);
      return Image;
   end Gtk_Image_New_From_Stock;

   --------------------------------
   -- Gtk_Image_New_From_Surface --
   --------------------------------

   function Gtk_Image_New_From_Surface
      (Surface : Cairo.Cairo_Surface) return Gtk_Image
   is
      Image : constant Gtk_Image := new Gtk_Image_Record;
   begin
      Gtk.Image.Initialize_From_Surface (Image, Surface);
      return Image;
   end Gtk_Image_New_From_Surface;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Image : out Gtk_Image) is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image     : out Gtk_Image;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Animation);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Image : out Gtk_Image; Filename : UTF8_String) is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Filename);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Icon_Set, Size);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Pixbuf);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Stock_Id, Size);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Gicon --
   ------------------------

   procedure Gtk_New_From_Gicon
      (Image : out Gtk_Image;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Gicon (Image, Icon, Size);
   end Gtk_New_From_Gicon;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
      (Image     : out Gtk_Image;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Icon_Name (Image, Icon_Name, Size);
   end Gtk_New_From_Icon_Name;

   ---------------------------
   -- Gtk_New_From_Resource --
   ---------------------------

   procedure Gtk_New_From_Resource
      (Image         : out Gtk_Image;
       Resource_Path : UTF8_String)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Resource (Image, Resource_Path);
   end Gtk_New_From_Resource;

   --------------------------
   -- Gtk_New_From_Surface --
   --------------------------

   procedure Gtk_New_From_Surface
      (Image   : out Gtk_Image;
       Surface : Cairo.Cairo_Surface)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Surface (Image, Surface);
   end Gtk_New_From_Surface;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Image : not null access Gtk_Image_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image     : not null access Gtk_Image_Record'Class;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      function Internal
         (Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_animation");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal (Animation));
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Filename : UTF8_String)
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      if not Image.Is_Created then
         Tmp_Return := Internal (Tmp_Filename);
         Free (Tmp_Filename);
         Set_Object (Image, Tmp_Return);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon_Set : System.Address;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_set");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal (Get_Object (Icon_Set), Size));
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image  : not null access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixbuf");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal (Get_Object_Or_Null (GObject (Pixbuf))));
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Stock_Id : Gtkada.Types.Chars_Ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      if not Image.Is_Created then
         Tmp_Return := Internal (Tmp_Stock_Id, Size);
         Free (Tmp_Stock_Id);
         Set_Object (Image, Tmp_Return);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Gicon --
   ---------------------------

   procedure Initialize_From_Gicon
      (Image : not null access Gtk_Image_Record'Class;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon : Glib.G_Icon.G_Icon;
          Size : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_gicon");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal (Icon, Size));
      end if;
   end Initialize_From_Gicon;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
      (Image     : not null access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon_Name : Gtkada.Types.Chars_Ptr;
          Size      : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return    : System.Address;
   begin
      if not Image.Is_Created then
         if Icon_Name = "" then
            Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
         else
            Tmp_Icon_Name := New_String (Icon_Name);
         end if;
         Tmp_Return := Internal (Tmp_Icon_Name, Size);
         Free (Tmp_Icon_Name);
         Set_Object (Image, Tmp_Return);
      end if;
   end Initialize_From_Icon_Name;

   ------------------------------
   -- Initialize_From_Resource --
   ------------------------------

   procedure Initialize_From_Resource
      (Image         : not null access Gtk_Image_Record'Class;
       Resource_Path : UTF8_String)
   is
      function Internal
         (Resource_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
      Tmp_Return        : System.Address;
   begin
      if not Image.Is_Created then
         Tmp_Return := Internal (Tmp_Resource_Path);
         Free (Tmp_Resource_Path);
         Set_Object (Image, Tmp_Return);
      end if;
   end Initialize_From_Resource;

   -----------------------------
   -- Initialize_From_Surface --
   -----------------------------

   procedure Initialize_From_Surface
      (Image   : not null access Gtk_Image_Record'Class;
       Surface : Cairo.Cairo_Surface)
   is
      function Internal
         (Surface : Cairo.Cairo_Surface) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_surface");
   begin
      if not Image.Is_Created then
         Set_Object (Image, Internal (Surface));
      end if;
   end Initialize_From_Surface;

   -----------
   -- Clear --
   -----------

   procedure Clear (Image : not null access Gtk_Image_Record) is
      procedure Internal (Image : System.Address);
      pragma Import (C, Internal, "gtk_image_clear");
   begin
      Internal (Get_Object (Image));
   end Clear;

   ---------
   -- Get --
   ---------

   function Get
      (Image : not null access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf_Animation
   is
      function Internal
         (Image : System.Address) return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
      pragma Import (C, Internal, "gtk_image_get_animation");
   begin
      return Internal (Get_Object (Image));
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image  : not null access Gtk_Image_Record;
       G_Icon : out Glib.G_Icon.G_Icon;
       Size   : out Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image  : System.Address;
          G_Icon : out Glib.G_Icon.G_Icon;
          Size   : out Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_gicon");
   begin
      Internal (Get_Object (Image), G_Icon, Size);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image    : not null access Gtk_Image_Record;
       Icon_Set : out Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : out Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Icon_Set : out System.Address;
          Size     : out Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_icon_set");
      Tmp_Icon_Set : aliased System.Address;
   begin
      Internal (Get_Object (Image), Tmp_Icon_Set, Size);
      Icon_Set := From_Object (Tmp_Icon_Set);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
      (Image : not null access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Image : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Image)), Stub_Gdk_Pixbuf));
   end Get;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   function Get_Pixel_Size
      (Image : not null access Gtk_Image_Record) return Glib.Gint
   is
      function Internal (Image : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_image_get_pixel_size");
   begin
      return Internal (Get_Object (Image));
   end Get_Pixel_Size;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
      (Image : not null access Gtk_Image_Record) return Gtk_Image_Type
   is
      function Internal (Image : System.Address) return Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_image_get_storage_type");
   begin
      return Internal (Get_Object (Image));
   end Get_Storage_Type;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image     : not null access Gtk_Image_Record;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      procedure Internal
         (Image     : System.Address;
          Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gtk_image_set_from_animation");
   begin
      Internal (Get_Object (Image), Animation);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Filename : UTF8_String := "")
   is
      procedure Internal
         (Image    : System.Address;
          Filename : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_image_set_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr;
   begin
      if Filename = "" then
         Tmp_Filename := Gtkada.Types.Null_Ptr;
      else
         Tmp_Filename := New_String (Filename);
      end if;
      Internal (Get_Object (Image), Tmp_Filename);
      Free (Tmp_Filename);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image : not null access Gtk_Image_Record;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image : System.Address;
          Icon  : Glib.G_Icon.G_Icon;
          Size  : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_gicon");
   begin
      Internal (Get_Object (Image), Icon, Size);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Icon_Set : System.Address;
          Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_set");
   begin
      Internal (Get_Object (Image), Get_Object (Icon_Set), Size);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image  : not null access Gtk_Image_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Image : System.Address; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_image_set_from_pixbuf");
   begin
      Internal (Get_Object (Image), Get_Object_Or_Null (GObject (Pixbuf)));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Stock_Id : Gtkada.Types.Chars_Ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Image), Tmp_Stock_Id, Size);
      Free (Tmp_Stock_Id);
   end Set;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
      (Image     : not null access Gtk_Image_Record;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image     : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr;
          Size      : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Image), Tmp_Icon_Name, Size);
      Free (Tmp_Icon_Name);
   end Set_From_Icon_Name;

   -----------------------
   -- Set_From_Resource --
   -----------------------

   procedure Set_From_Resource
      (Image         : not null access Gtk_Image_Record;
       Resource_Path : UTF8_String := "")
   is
      procedure Internal
         (Image         : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_image_set_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr;
   begin
      if Resource_Path = "" then
         Tmp_Resource_Path := Gtkada.Types.Null_Ptr;
      else
         Tmp_Resource_Path := New_String (Resource_Path);
      end if;
      Internal (Get_Object (Image), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
   end Set_From_Resource;

   ----------------------
   -- Set_From_Surface --
   ----------------------

   procedure Set_From_Surface
      (Image   : not null access Gtk_Image_Record;
       Surface : Cairo.Cairo_Surface)
   is
      procedure Internal
         (Image   : System.Address;
          Surface : Cairo.Cairo_Surface);
      pragma Import (C, Internal, "gtk_image_set_from_surface");
   begin
      Internal (Get_Object (Image), Surface);
   end Set_From_Surface;

   --------------------
   -- Set_Pixel_Size --
   --------------------

   procedure Set_Pixel_Size
      (Image      : not null access Gtk_Image_Record;
       Pixel_Size : Glib.Gint)
   is
      procedure Internal (Image : System.Address; Pixel_Size : Glib.Gint);
      pragma Import (C, Internal, "gtk_image_set_pixel_size");
   begin
      Internal (Get_Object (Image), Pixel_Size);
   end Set_Pixel_Size;

end Gtk.Image;

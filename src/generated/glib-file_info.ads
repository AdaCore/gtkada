------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Functionality for manipulating basic metadata for files.
--  Glib.File_Info.Gfile_Info implements methods for getting information that
--  all files should contain, and allows for manipulation of extended
--  attributes.
--
--  See [GFileAttribute][gio-GFileAttribute] for more information on how GIO
--  handles file attributes.
--
--  To obtain a Glib.File_Info.Gfile_Info for a Glib.GFile.Gfile, use
--  Glib.GFile.Query_Info (or its async variant). To obtain a
--  Glib.File_Info.Gfile_Info for a file input or output stream, use
--  Glib.File_Input_Stream.Query_Info or Glib.File_Output_Stream.Query_Info (or
--  their async variants).
--
--  To change the actual attributes of a file, you should then set the
--  attribute in the Glib.File_Info.Gfile_Info and call
--  Glib.GFile.Set_Attributes_From_Info or Glib.GFile.Set_Attributes_Async on a
--  GFile.
--
--  However, not all attributes can be changed in the file. For instance, the
--  actual size of a file cannot be changed via Glib.File_Info.Set_Size. You
--  may call g_file_query_settable_attributes and
--  g_file_query_writable_namespaces to discover the settable attributes of a
--  particular file at runtime.
--
--  Gfile.Attribute_Matcher.Gfile_Attribute_Matcher allows for searching
--  through a Glib.File_Info.Gfile_Info for attributes.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;

package Glib.File_Info is

   type Gfile_Info_Record is new GObject_Record with null record;
   type Gfile_Info is access all Gfile_Info_Record'Class;

   type GFile_Type is (
      G_File_Type_Unknown,
      G_File_Type_Regular,
      G_File_Type_Directory,
      G_File_Type_Symbolic_Link,
      G_File_Type_Special,
      G_File_Type_Shortcut,
      G_File_Type_Mountable);
   pragma Convention (C, GFile_Type);
   --  Indicates the file's on-disk type.
   --
   --  On Windows systems a file will never have
   --  Glib.File_Info.G_File_Type_Symbolic_Link type; use
   --  Glib.File_Info.Gfile_Info and G_FILE_ATTRIBUTE_STANDARD_IS_SYMLINK to
   --  determine whether a file is a symlink or not. This is due to the fact
   --  that NTFS does not have a single filesystem object type for symbolic
   --  links - it has files that symlink to files, and directories that symlink
   --  to directories. Glib.File_Info.GFile_Type enumeration cannot precisely
   --  represent this important distinction, which is why all Windows symlinks
   --  will continue to be reported as Glib.File_Info.G_File_Type_Regular or
   --  Glib.File_Info.G_File_Type_Directory.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GFile_Type_Properties is
      new Generic_Internal_Discrete_Property (GFile_Type);
   type Property_GFile_Type is new GFile_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gfile_Info);
   --  Creates a new file info structure.

   procedure Initialize (Self : not null access Gfile_Info_Record'Class);
   --  Creates a new file info structure.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gfile_Info_New return Gfile_Info;
   --  Creates a new file info structure.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_file_info_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear_Status (Self : not null access Gfile_Info_Record);
   --  Clears the status information from Info.

   procedure Copy_Into
      (Self      : not null access Gfile_Info_Record;
       Dest_Info : not null access Gfile_Info_Record'Class);
   --  First clears all of the [GFileAttribute][gio-GFileAttribute] of
   --  Dest_Info, and then copies all of the file attributes from Src_Info to
   --  Dest_Info.
   --  @param Dest_Info destination to copy attributes to.

   function Dup (Self : not null access Gfile_Info_Record) return Gfile_Info;
   --  Duplicates a file info structure.
   --  @return a duplicate Glib.File_Info.Gfile_Info of Other.

   function Get_Attribute_As_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String;
   --  Gets the value of a attribute, formatted as a string. This escapes
   --  things as needed to make the string valid UTF-8.
   --  @param Attribute a file attribute key.
   --  @return a UTF-8 string associated with the given Attribute, or null if
   --  the attribute wasn't set. When you're done with the string it must be
   --  freed with g_free.

   function Get_Attribute_Boolean
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Boolean;
   --  Gets the value of a boolean attribute. If the attribute does not
   --  contain a boolean value, False will be returned.
   --  @param Attribute a file attribute key.
   --  @return the boolean value contained within the attribute.

   procedure Set_Attribute_Boolean
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Boolean);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value a boolean value.

   function Get_Attribute_Byte_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String;
   --  Gets the value of a byte string attribute. If the attribute does not
   --  contain a byte string, null will be returned.
   --  @param Attribute a file attribute key.
   --  @return the contents of the Attribute value as a byte string, or null
   --  otherwise.

   procedure Set_Attribute_Byte_String
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : UTF8_String);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value a byte string.

   function Get_Attribute_Int32
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Gint32;
   --  Gets a signed 32-bit integer contained within the attribute. If the
   --  attribute does not contain a signed 32-bit integer, or is invalid, 0
   --  will be returned.
   --  @param Attribute a file attribute key.
   --  @return a signed 32-bit integer from the attribute.

   procedure Set_Attribute_Int32
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Gint32);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value a signed 32-bit integer

   function Get_Attribute_Int64
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Gint64;
   --  Gets a signed 64-bit integer contained within the attribute. If the
   --  attribute does not contain a signed 64-bit integer, or is invalid, 0
   --  will be returned.
   --  @param Attribute a file attribute key.
   --  @return a signed 64-bit integer from the attribute.

   procedure Set_Attribute_Int64
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Gint64);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute attribute name to set.
   --  @param Attr_Value int64 value to set attribute to.

   function Get_Attribute_Object
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Glib.Object.GObject;
   --  Gets the value of a Glib.Object.GObject attribute. If the attribute
   --  does not contain a Glib.Object.GObject, null will be returned.
   --  @param Attribute a file attribute key.
   --  @return a Glib.Object.GObject associated with the given Attribute, or
   --  null otherwise.

   procedure Set_Attribute_Object
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : not null access Glib.Object.GObject_Record'Class);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value a Glib.Object.GObject.

   function Get_Attribute_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String;
   --  Gets the value of a string attribute. If the attribute does not contain
   --  a string, null will be returned.
   --  @param Attribute a file attribute key.
   --  @return the contents of the Attribute value as a UTF-8 string, or null
   --  otherwise.

   procedure Set_Attribute_String
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : UTF8_String);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value a UTF-8 string.

   function Get_Attribute_Stringv
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return GNAT.Strings.String_List;
   --  Gets the value of a stringv attribute. If the attribute does not
   --  contain a stringv, null will be returned.
   --  Since: gtk+ 2.22
   --  @param Attribute a file attribute key.
   --  @return the contents of the Attribute value as a stringv, or null
   --  otherwise. Do not free. These returned strings are UTF-8.

   procedure Set_Attribute_Stringv
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : GNAT.Strings.String_List);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  Sinze: 2.22
   --  @param Attribute a file attribute key
   --  @param Attr_Value a null terminated array of UTF-8 strings.

   function Get_Attribute_Uint32
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Guint32;
   --  Gets an unsigned 32-bit integer contained within the attribute. If the
   --  attribute does not contain an unsigned 32-bit integer, or is invalid, 0
   --  will be returned.
   --  @param Attribute a file attribute key.
   --  @return an unsigned 32-bit integer from the attribute.

   procedure Set_Attribute_Uint32
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Guint32);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value an unsigned 32-bit integer.

   function Get_Attribute_Uint64
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Guint64;
   --  Gets a unsigned 64-bit integer contained within the attribute. If the
   --  attribute does not contain an unsigned 64-bit integer, or is invalid, 0
   --  will be returned.
   --  @param Attribute a file attribute key.
   --  @return a unsigned 64-bit integer from the attribute.

   procedure Set_Attribute_Uint64
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Guint64);
   --  Sets the Attribute to contain the given Attr_Value, if possible.
   --  @param Attribute a file attribute key.
   --  @param Attr_Value an unsigned 64-bit integer.

   function Get_Content_Type
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets the file's content type.
   --  @return a string containing the file's content type, or null if
   --  unknown.

   procedure Set_Content_Type
      (Self         : not null access Gfile_Info_Record;
       Content_Type : UTF8_String);
   --  Sets the content type attribute for a given Glib.File_Info.Gfile_Info.
   --  See G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE.
   --  @param Content_Type a content type. See
   --  [GContentType][gio-GContentType]

   function Get_Display_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets a display name for a file. This is guaranteed to always be set.
   --  @return a string containing the display name.

   procedure Set_Display_Name
      (Self         : not null access Gfile_Info_Record;
       Display_Name : UTF8_String);
   --  Sets the display name for the current Glib.File_Info.Gfile_Info. See
   --  G_FILE_ATTRIBUTE_STANDARD_DISPLAY_NAME.
   --  @param Display_Name a string containing a display name.

   function Get_Edit_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets the edit name for a file.
   --  @return a string containing the edit name.

   procedure Set_Edit_Name
      (Self      : not null access Gfile_Info_Record;
       Edit_Name : UTF8_String);
   --  Sets the edit name for the current file. See
   --  G_FILE_ATTRIBUTE_STANDARD_EDIT_NAME.
   --  @param Edit_Name a string containing an edit name.

   function Get_Etag
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets the [entity tag][gfile-etag] for a given
   --  Glib.File_Info.Gfile_Info. See G_FILE_ATTRIBUTE_ETAG_VALUE.
   --  @return a string containing the value of the "etag:value" attribute.

   function Get_File_Type
      (Self : not null access Gfile_Info_Record) return GFile_Type;
   --  Gets a file's type (whether it is a regular file, symlink, etc). This
   --  is different from the file's content type, see
   --  Glib.File_Info.Get_Content_Type.
   --  @return a Glib.File_Info.GFile_Type for the given file.

   procedure Set_File_Type
      (Self     : not null access Gfile_Info_Record;
       The_Type : GFile_Type);
   --  Sets the file type in a Glib.File_Info.Gfile_Info to Type. See
   --  G_FILE_ATTRIBUTE_STANDARD_TYPE.
   --  @param The_Type a Glib.File_Info.GFile_Type.

   function Get_Icon
      (Self : not null access Gfile_Info_Record) return Glib.G_Icon.G_Icon;
   --  Gets the icon for a file.
   --  @return Glib.G_Icon.G_Icon for the given Info.

   procedure Set_Icon
      (Self : not null access Gfile_Info_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets the icon for a given Glib.File_Info.Gfile_Info. See
   --  G_FILE_ATTRIBUTE_STANDARD_ICON.
   --  @param Icon a Glib.G_Icon.G_Icon.

   function Get_Is_Backup
      (Self : not null access Gfile_Info_Record) return Boolean;
   --  Checks if a file is a backup file.
   --  @return True if file is a backup file, False otherwise.

   function Get_Is_Hidden
      (Self : not null access Gfile_Info_Record) return Boolean;
   --  Checks if a file is hidden.
   --  @return True if the file is a hidden file, False otherwise.

   procedure Set_Is_Hidden
      (Self      : not null access Gfile_Info_Record;
       Is_Hidden : Boolean);
   --  Sets the "is_hidden" attribute in a Glib.File_Info.Gfile_Info according
   --  to Is_Hidden. See G_FILE_ATTRIBUTE_STANDARD_IS_HIDDEN.
   --  @param Is_Hidden a Boolean.

   function Get_Is_Symlink
      (Self : not null access Gfile_Info_Record) return Boolean;
   --  Checks if a file is a symlink.
   --  @return True if the given Info is a symlink.

   procedure Set_Is_Symlink
      (Self       : not null access Gfile_Info_Record;
       Is_Symlink : Boolean);
   --  Sets the "is_symlink" attribute in a Glib.File_Info.Gfile_Info
   --  according to Is_Symlink. See G_FILE_ATTRIBUTE_STANDARD_IS_SYMLINK.
   --  @param Is_Symlink a Boolean.

   function Get_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets the name for a file. This is guaranteed to always be set.
   --  @return a string containing the file name.

   procedure Set_Name
      (Self : not null access Gfile_Info_Record;
       Name : UTF8_String);
   --  Sets the name attribute for the current Glib.File_Info.Gfile_Info. See
   --  G_FILE_ATTRIBUTE_STANDARD_NAME.
   --  @param Name a string containing a name.

   function Get_Size
      (Self : not null access Gfile_Info_Record) return Glib.Gint64;
   --  Gets the file's size.

   procedure Set_Size
      (Self : not null access Gfile_Info_Record;
       Size : Glib.Gint64);
   --  Sets the G_FILE_ATTRIBUTE_STANDARD_SIZE attribute in the file info to
   --  the given size.
   --  @param Size a goffset containing the file's size.

   function Get_Sort_Order
      (Self : not null access Gfile_Info_Record) return Gint32;
   --  Gets the value of the sort_order attribute from the
   --  Glib.File_Info.Gfile_Info. See G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER.
   --  @return a Gint32 containing the value of the "standard::sort_order"
   --  attribute.

   procedure Set_Sort_Order
      (Self       : not null access Gfile_Info_Record;
       Sort_Order : Gint32);
   --  Sets the sort order attribute in the file info structure. See
   --  G_FILE_ATTRIBUTE_STANDARD_SORT_ORDER.
   --  @param Sort_Order a sort order integer.

   function Get_Symbolic_Icon
      (Self : not null access Gfile_Info_Record) return Glib.G_Icon.G_Icon;
   --  Gets the symbolic icon for a file.
   --  Since: gtk+ 2.34
   --  @return Glib.G_Icon.G_Icon for the given Info.

   procedure Set_Symbolic_Icon
      (Self : not null access Gfile_Info_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets the symbolic icon for a given Glib.File_Info.Gfile_Info. See
   --  G_FILE_ATTRIBUTE_STANDARD_SYMBOLIC_ICON.
   --  Since: gtk+ 2.34
   --  @param Icon a Glib.G_Icon.G_Icon.

   function Get_Symlink_Target
      (Self : not null access Gfile_Info_Record) return UTF8_String;
   --  Gets the symlink target for a given Glib.File_Info.Gfile_Info.
   --  @return a string containing the symlink target.

   procedure Set_Symlink_Target
      (Self           : not null access Gfile_Info_Record;
       Symlink_Target : UTF8_String);
   --  Sets the G_FILE_ATTRIBUTE_STANDARD_SYMLINK_TARGET attribute in the file
   --  info to the given symlink target.
   --  @param Symlink_Target a static string containing a path to a symlink
   --  target.

   function Has_Attribute
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Boolean;
   --  Checks if a file info structure has an attribute named Attribute.
   --  @param Attribute a file attribute key.
   --  @return True if Info has an attribute named Attribute, False otherwise.

   function Has_Namespace
      (Self       : not null access Gfile_Info_Record;
       Name_Space : UTF8_String) return Boolean;
   --  Checks if a file info structure has an attribute in the specified
   --  Name_Space.
   --  Since: gtk+ 2.22
   --  @param Name_Space a file attribute namespace.
   --  @return True if Info has an attribute in Name_Space, False otherwise.

   function List_Attributes
      (Self       : not null access Gfile_Info_Record;
       Name_Space : UTF8_String := "") return GNAT.Strings.String_List;
   --  Lists the file info structure's attributes.
   --  @param Name_Space a file attribute key's namespace, or null to list all
   --  attributes.
   --  @return a null-terminated array of strings of all of the possible
   --  attribute types for the given Name_Space, or null on error.

   procedure Remove_Attribute
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String);
   --  Removes all cases of Attribute from Info if it exists.
   --  @param Attribute a file attribute key.

   procedure Unset_Attribute_Mask (Self : not null access Gfile_Info_Record);
   --  Unsets a mask set by g_file_info_set_attribute_mask, if one is set.

end Glib.File_Info;

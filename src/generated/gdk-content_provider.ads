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

--  Provides content for the clipboard or for drag-and-drop operations in a
--  number of formats.
--
--  To create a `GdkContentProvider`, use
--  [ctorGdk.ContentProvider.new_for_value] or
--  [ctorGdk.ContentProvider.new_for_bytes].
--
--  GDK knows how to handle common text and image formats out-of-the-box. See
--  [classGdk.ContentSerializer] and [classGdk.ContentDeserializer] if you want
--  to add support for application-specific data formats.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Formats; use Gdk.Content_Formats;
with Glib;                use Glib;
with Glib.Bytes;          use Glib.Bytes;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Values;         use Glib.Values;

package Gdk.Content_Provider is

   type Gdk_Content_Provider_Record is new GObject_Record with null record;
   type Gdk_Content_Provider is access all Gdk_Content_Provider_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New_For_Bytes
      (Self      : out Gdk_Content_Provider;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes);
   --  Create a content provider that provides the given Bytes as data for the
   --  given Mime_Type.
   --  @param Mime_Type the mime type
   --  @param Bytes a `GBytes` with the data for Mime_Type

   procedure Initialize_For_Bytes
      (Self      : not null access Gdk_Content_Provider_Record'Class;
       Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes);
   --  Create a content provider that provides the given Bytes as data for the
   --  given Mime_Type.
   --  Initialize_For_Bytes does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Mime_Type the mime type
   --  @param Bytes a `GBytes` with the data for Mime_Type

   function Gdk_Content_Provider_New_For_Bytes
      (Mime_Type : UTF8_String;
       Bytes     : Glib.Bytes.Gbytes) return Gdk_Content_Provider;
   --  Create a content provider that provides the given Bytes as data for the
   --  given Mime_Type.
   --  @param Mime_Type the mime type
   --  @param Bytes a `GBytes` with the data for Mime_Type

   procedure Gdk_New_For_Value
      (Self  : out Gdk_Content_Provider;
       Value : in out Glib.Values.GValue);
   --  Create a content provider that provides the given Value.
   --  @param Value a `GValue`

   procedure Initialize_For_Value
      (Self  : not null access Gdk_Content_Provider_Record'Class;
       Value : in out Glib.Values.GValue);
   --  Create a content provider that provides the given Value.
   --  Initialize_For_Value does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Value a `GValue`

   function Gdk_Content_Provider_New_For_Value
      (Value : in out Glib.Values.GValue) return Gdk_Content_Provider;
   --  Create a content provider that provides the given Value.
   --  @param Value a `GValue`

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_content_provider_get_type");

   -------------
   -- Methods --
   -------------

   procedure Content_Changed
      (Self : not null access Gdk_Content_Provider_Record);
   --  Emits the ::content-changed signal.

   function Get_Value
      (Self  : not null access Gdk_Content_Provider_Record;
       Value : access Glib.Values.GValue) return Boolean;
   --  Gets the contents of Provider stored in Value.
   --  The Value will have been initialized to the `GType` the value should be
   --  provided in. This given `GType` does not need to be listed in the
   --  formats returned by [methodGdk.ContentProvider.ref_formats]. However, if
   --  the given `GType` is not supported, this operation can fail and
   --  `G_IO_ERROR_NOT_SUPPORTED` will be reported.
   --  @param Value the `GValue` to fill
   --  @return True if the value was set successfully. Otherwise Error will be
   --  set to describe the failure.

   function Ref_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Gets the formats that the provider can provide its current contents in.
   --  @return The formats of the provider

   function Ref_Storable_Formats
      (Self : not null access Gdk_Content_Provider_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Gets the formats that the provider suggests other applications to store
   --  the data in.
   --  An example of such an application would be a clipboard manager.
   --  This can be assumed to be a subset of
   --  [methodGdk.ContentProvider.ref_formats].
   --  @return The storable formats of the provider

   function Write_Mime_Type_Finish
      (Self   : not null access Gdk_Content_Provider_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous write operation.
   --  See [methodGdk.ContentProvider.write_mime_type_async].
   --  @param Result a `GAsyncResult`
   --  @return True if the operation was completed successfully. Otherwise
   --  Error will be set to describe the failure.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Formats
   --  The possible formats that the provider can provide its data in.

   Storable_Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Formats
   --  The subset of formats that clipboard managers should store this
   --  provider's data in.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Content_Provider_Void is not null access procedure
     (Self : access Gdk_Content_Provider_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Content_Changed : constant Glib.Signal_Name := "content-changed";
   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_Gdk_Content_Provider_Void;
       After : Boolean := False);
   procedure On_Content_Changed
      (Self  : not null access Gdk_Content_Provider_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the content provided by this provider has changed.

private
   Storable_Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("storable-formats");
   Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("formats");
end Gdk.Content_Provider;

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
with Gtkada.Bindings; use Gtkada.Bindings;

package body Glib.IOChannel is

   function From_Object_Free (B : access GIOFuncs) return GIOFuncs is
      Result : constant GIOFuncs := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access GIO_Channel_Record) return GIO_Channel_Record is
      Result : constant GIO_Channel_Record := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ------------------
   -- Set_Encoding --
   ------------------

   function Set_Encoding
     (Self     : Giochannel;
      Encoding : UTF8_String := "";
      Error    : access Glib.Error.GError) return GIOStatus
   is
      function Internal
        (Self     : Giochannel;
         Encoding : Gtkada.Types.Chars_Ptr;
         Error    : access Glib.Error.GError) return GIOStatus;
      pragma Import (C, Internal, "g_io_channel_set_encoding");
      Tmp_Encoding : Gtkada.Types.Chars_Ptr;
      Tmp_Return   : GIOStatus;
   begin
      if Encoding = "" then
         Tmp_Encoding := Gtkada.Types.Null_Ptr;
      else
         Tmp_Encoding := New_String (Encoding);
      end if;
      Tmp_Return := Internal (Self, Tmp_Encoding, Error);
      Free (Tmp_Encoding);
      return Tmp_Return;
   end Set_Encoding;

   ----------------
   -- Read_Chars --
   ----------------

   function Read_Chars
     (Self       : Giochannel;
      Buf        : out Ada.Streams.Stream_Element_Array;
      Bytes_Read : access Gsize;
      Error    : access Glib.Error.GError) return GIOStatus
   is
      function Internal
        (Self           : Giochannel;
         Buf            : System.Address;
         Count          : Gsize;
         Acc_Bytes_Read : access Gsize;
         Error          : access Glib.Error.GError) return GIOStatus;
      pragma Import (C, Internal, "g_io_channel_read_chars");
      Acc_Bytes_Read : aliased Gsize;
      Tmp_Return     : GIOStatus;
   begin
      Tmp_Return := Internal (Self, Buf'Address, Buf'Length, Acc_Bytes_Read'Access, Error);
      if Bytes_Read /= null then
         Bytes_Read.all := Acc_Bytes_Read;
      end if;
      return Tmp_Return;
   end Read_Chars;

   -----------------
   -- Write_Chars --
   -----------------

   function Write_Chars
     (Self          : Giochannel;
      Buf           : Ada.Streams.Stream_Element_Array;
      Bytes_Written : access Gsize;
      Error         : access Glib.Error.GError) return GIOStatus
   is
      function Internal
        (Self              : Giochannel;
         Buf               : System.Address;
         Count             : Gssize;
         Acc_Bytes_Written : access Gsize;
         Error             : access Glib.Error.GError) return GIOStatus;
      pragma Import (C, Internal, "g_io_channel_write_chars");
      Acc_Bytes_Written : aliased Gsize;
      Tmp_Return        : GIOStatus;
   begin
      Tmp_Return := Internal (Self, Buf'Address, Buf'Length, Acc_Bytes_Written'Access, Error);
      Bytes_Written.all := Acc_Bytes_Written;
      return Tmp_Return;
   end Write_Chars;

   ----------------
   -- G_Unix_New --
   ----------------

   procedure G_Unix_New (Self : out Giochannel; Fd : Glib.Gint) is
      function Internal (Fd : Glib.Gint) return Giochannel;
      pragma Import (C, Internal, "g_io_channel_unix_new");
   begin
      Self := Internal (Fd);
   end G_Unix_New;

   -------------------------
   -- Giochannel_Unix_New --
   -------------------------

   function Giochannel_Unix_New (Fd : Glib.Gint) return Giochannel is
      function Internal (Fd : Glib.Gint) return Giochannel;
      pragma Import (C, Internal, "g_io_channel_unix_new");
      Self : Giochannel;
   begin
      Self := Internal (Fd);
      return Self;
   end Giochannel_Unix_New;

   ------------------
   -- Get_Buffered --
   ------------------

   function Get_Buffered (Self : Giochannel) return Boolean is
      function Internal (Self : Giochannel) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_channel_get_buffered");
   begin
      return Internal (Self) /= 0;
   end Get_Buffered;

   ------------------------
   -- Get_Close_On_Unref --
   ------------------------

   function Get_Close_On_Unref (Self : Giochannel) return Boolean is
      function Internal (Self : Giochannel) return Glib.Gboolean;
      pragma Import (C, Internal, "g_io_channel_get_close_on_unref");
   begin
      return Internal (Self) /= 0;
   end Get_Close_On_Unref;

   ------------------
   -- Get_Encoding --
   ------------------

   function Get_Encoding (Self : Giochannel) return UTF8_String is
      function Internal (Self : Giochannel) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_io_channel_get_encoding");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Encoding;

   -------------------
   -- Get_Line_Term --
   -------------------

   function Get_Line_Term
      (Self   : Giochannel;
       Length : in out Glib.Gint) return UTF8_String
   is
      function Internal
         (Self       : Giochannel;
          Acc_Length : access Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_io_channel_get_line_term");
      Acc_Length : aliased Glib.Gint := Length;
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Self, Acc_Length'Access);
      Length := Acc_Length;
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_Line_Term;

   ------------------
   -- Set_Buffered --
   ------------------

   procedure Set_Buffered (Self : Giochannel; Buffered : Boolean) is
      procedure Internal (Self : Giochannel; Buffered : Glib.Gboolean);
      pragma Import (C, Internal, "g_io_channel_set_buffered");
   begin
      Internal (Self, Boolean'Pos (Buffered));
   end Set_Buffered;

   ------------------------
   -- Set_Close_On_Unref --
   ------------------------

   procedure Set_Close_On_Unref (Self : Giochannel; Do_Close : Boolean) is
      procedure Internal (Self : Giochannel; Do_Close : Glib.Gboolean);
      pragma Import (C, Internal, "g_io_channel_set_close_on_unref");
   begin
      Internal (Self, Boolean'Pos (Do_Close));
   end Set_Close_On_Unref;

   -------------------
   -- Set_Line_Term --
   -------------------

   procedure Set_Line_Term
      (Self      : Giochannel;
       Line_Term : UTF8_String := "";
       Length    : Glib.Gint)
   is
      procedure Internal
         (Self      : Giochannel;
          Line_Term : Gtkada.Types.Chars_Ptr;
          Length    : Glib.Gint);
      pragma Import (C, Internal, "g_io_channel_set_line_term");
      Tmp_Line_Term : Gtkada.Types.Chars_Ptr;
   begin
      if Line_Term = "" then
         Tmp_Line_Term := Gtkada.Types.Null_Ptr;
      else
         Tmp_Line_Term := New_String (Line_Term);
      end if;
      Internal (Self, Tmp_Line_Term, Length);
      Free (Tmp_Line_Term);
   end Set_Line_Term;

end Glib.IOChannel;

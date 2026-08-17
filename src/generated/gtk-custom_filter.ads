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

--  Determines whether to include items with a callback.

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Gtk.Filter; use Gtk.Filter;

package Gtk.Custom_Filter is

   type Gtk_Custom_Filter_Record is new Gtk_Filter_Record with null record;
   type Gtk_Custom_Filter is access all Gtk_Custom_Filter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_custom_filter_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Custom_Filter_Func is access function (Item : Glib.Object.GObject) return Boolean;
   --  User function that is called to determine if the Item should be
   --  matched.
   --  If the filter matches the item, this function must return true. If the
   --  item should be filtered out, false must be returned.
   --  @param Item the item to be matched
   --  @return true to keep the item around

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
     (Self       : out Gtk_Custom_Filter;
      Match_Func : Gtk_Custom_Filter_Func);
   procedure Initialize
     (Self       : not null access Gtk_Custom_Filter_Record'Class;
      Match_Func : Gtk_Custom_Filter_Func);
   --  Creates a new filter using the given function to filter items.
   --  If Match_Func is `NULL`, the filter matches all items.
   --  If the filter func changes its filtering behavior,
   --  [methodGtk.Filter.changed] needs to be called.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Match_Func function to filter items

   function Gtk_Custom_Filter_New
     (Match_Func : Gtk_Custom_Filter_Func)
   return Gtk_Custom_Filter;
   --  Creates a new filter using the given function to filter items.
   --  If Match_Func is `NULL`, the filter matches all items.
   --  If the filter func changes its filtering behavior,
   --  [methodGtk.Filter.changed] needs to be called.
   --  @param Match_Func function to filter items

   procedure Set_Filter_Func
     (Self       : not null access Gtk_Custom_Filter_Record;
      Match_Func : Gtk_Custom_Filter_Func);
   --  Sets the function used for filtering items.
   --  If Match_Func is `NULL`, the filter matches all items.
   --  If the filter func changes its filtering behavior,
   --  [methodGtk.Filter.changed] needs to be called.
   --  If a previous function was set, its User_Destroy will be called.
   --  @param Match_Func function to filter items

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Custom_Filter_User_Data is

      type Gtk_Custom_Filter_Func_User is access function
        (Item      : Glib.Object.GObject;
         User_Data : User_Data_Type) return Boolean;
      --  User function that is called to determine if the Item should be
      --  matched.
      --  If the filter matches the item, this function must return true. If the
      --  item should be filtered out, false must be returned.
      --  @param Item the item to be matched
      --  @param User_Data user data
      --  @return true to keep the item around

      procedure Gtk_New_User
        (Self       : out Gtk_Custom_Filter;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type);
      procedure Initialize_User
        (Self       : not null access Gtk_Custom_Filter_Record'Class;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type);
      --  Creates a new filter using the given function to filter items.
      --  If Match_Func is `NULL`, the filter matches all items.
      --  If the filter func changes its filtering behavior,
      --  [methodGtk.Filter.changed] needs to be called.
      --  Initialize does nothing if the object was already created with another
      --  call to Initialize* or G_New.
      --  @param Match_Func function to filter items
      --  @param User_Data user data to pass to Match_Func

      function Gtk_Custom_Filter_New_User
        (Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type)
      return Gtk_Custom_Filter;
      --  Creates a new filter using the given function to filter items.
      --  If Match_Func is `NULL`, the filter matches all items.
      --  If the filter func changes its filtering behavior,
      --  [methodGtk.Filter.changed] needs to be called.
      --  @param Match_Func function to filter items

      procedure Set_Filter_Func_User
        (Self       : not null access Gtk.Custom_Filter.Gtk_Custom_Filter_Record'Class;
         Match_Func : Gtk_Custom_Filter_Func_User;
         User_Data  : User_Data_Type);
      --  Sets the function used for filtering items.
      --  If Match_Func is `NULL`, the filter matches all items.
      --  If the filter func changes its filtering behavior,
      --  [methodGtk.Filter.changed] needs to be called.
      --  If a previous function was set, its User_Destroy will be called.
      --  @param Match_Func function to filter items
      --  @param User_Data user data to pass to Match_Func

   end Custom_Filter_User_Data;

end Gtk.Custom_Filter;

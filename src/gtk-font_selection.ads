-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gdk.Font;
with Gtk.Button; use Gtk.Button;
with Gtk.Notebook;
with Gtk.Window;

package Gtk.Font_Selection is

   type Gtk_Font_Selection is new Gtk.Notebook.Gtk_Notebook with private;
   type Gtk_Font_Selection_Dialog is new Gtk.Window.Gtk_Window with private;


   type Gtk_Font_Filter_Type is (Font_Filter_Base,
                                 Font_Filter_User);
   --  These are the two types of filter available - base and user. The
   --  base filter is set by the application and can't be changed by the
   --  user.

   type Gtk_Font_Type is (Font_Bitmap,
                          Font_Scalable,
                          Font_Scalable_Bitmap,
                          Font_All);
   --  Used for determining the type of a font style, and also for setting
   --  filters.  These can be combined if a style has bitmaps and scalable
   --  fonts available.


   --------------------------------------
   --  Font_Selection_Dialog functions --
   --------------------------------------

   function Get_Font (Fsd    : in Gtk_Font_Selection_Dialog)
                      return      Gdk.Font.Gdk_Font;

   function Get_Font_Name (Fsd    : in Gtk_Font_Selection_Dialog)
                           return      String;

   function Get_Preview_Text (Fsd    : in Gtk_Font_Selection_Dialog)
                              return      String;

   procedure Set_Filter
      (Fsd         : in Gtk_Font_Selection_Dialog;
       Filter_Type : in Gtk_Font_Filter_Type;
       Font_Type   : in Gtk_Font_Type;
       Foundries   : in String;
       Weights     : in String;
       Slants      : in String;
       Setwidths   : in String;
       Spacings    : in String;
       Charsets    : in String);

   function Set_Font_Name
      (Fsd      : in Gtk_Font_Selection_Dialog;
       Fontname : in String)
       return        Boolean;

   procedure Set_Preview_Text
      (Fsd  : in Gtk_Font_Selection_Dialog;
       Text : in String);

   function Get_Cancel_Button (Fsd : in Gtk_Font_Selection_Dialog)
                               return Gtk.Button.Gtk_Button;
   function Get_OK_Button (Fsd : in Gtk_Font_Selection_Dialog)
                           return Gtk.Button.Gtk_Button;
   function Get_Apply_Button (Fsd : in Gtk_Font_Selection_Dialog)
                              return Gtk.Button.Gtk_Button;

   procedure Gtk_New (Widget : in out Gtk_Font_Selection_Dialog;
                      Title : String);

   -------------------------------
   --  Font_Selection functions --
   -------------------------------

   function Get_Font (Fontsel : in Gtk_Font_Selection)
                      return       Gdk.Font.Gdk_Font;

   function Get_Font_Name (Fontsel : in Gtk_Font_Selection)
                           return       String;

   function Get_Preview_Text (Fontsel : in Gtk_Font_Selection)
                              return       String;

   procedure Gtk_New (Widget : out Gtk_Font_Selection);

   procedure Set_Filter
      (Fontsel     : in Gtk_Font_Selection;
       Filter_Type : in Gtk_Font_Filter_Type;
       Font_Type   : in Gtk_Font_Type;
       Foundries   : in String;
       Weights     : in String;
       Slants      : in String;
       Setwidths   : in String;
       Spacings    : in String;
       Charsets    : in String);

   function Set_Font_Name
      (Fontsel  : in Gtk_Font_Selection;
       Fontname : in String)
       return        Boolean;

   procedure Set_Preview_Text
      (Fontsel : in Gtk_Font_Selection;
       Text    : in String);

private
   type Gtk_Font_Selection is new Gtk.Notebook.Gtk_Notebook with null record;
   type Gtk_Font_Selection_Dialog is new Gtk.Window.Gtk_Window
     with null record;

   for Gtk_Font_Type use (Font_Bitmap          => 1,
                          Font_Scalable        => 2,
                          Font_Scalable_Bitmap => 4,
                          Font_All             => 7);

end Gtk.Font_Selection;

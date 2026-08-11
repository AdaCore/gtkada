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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Try_Expression is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self        : out Gtk_Try_Expression;
      Expressions : Gtk.Expression.Gtk_Expression_Array)
   is
   begin
      Self := new Gtk_Try_Expression_Record;
      Gtk.Try_Expression.Initialize (Self, Expressions);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Gtk_Try_Expression_Record'Class;
      Expressions : Gtk.Expression.Gtk_Expression_Array)
   is
      function Internal
        (N_Expressions   : Guint;
         Acc_Expressions : System.Address)
      return System.Address;
      pragma Import (C, Internal, "gtk_try_expression_new");
      Acc_Expressions : Gtk.Expression.C_GtkExpressionArray :=
      Gtk.Expression.To_C (Expressions);
   begin
      if not Self.Is_Created then
         --  transfer-ownership="full"
         for Expression of Expressions loop
            if Expression /= null then
               Adjust (Expression.all);
            end if;
         end loop;

         Set_Object (Self, Internal (Guint (Acc_Expressions'Length), Acc_Expressions'Address));
      end if;
   end Initialize;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Try_Expression_Record
   is
      pragma Unreferenced (Object);
      Result : Gtk_Try_Expression_Record;
   begin
      return Result;
   end Create;

end Gtk.Try_Expression;

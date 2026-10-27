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

--  Specifies a transfer function for a color component to be applied while
--  rendering.
--
--  The available functions include linear, piecewise-linear, gamma and step
--  functions.
--
--  Note that the transfer function is applied to un-premultiplied values, and
--  all results are clamped to the [0, 1] range.

pragma Warnings (Off, "*is already use-visible*");
with Glib;         use Glib;
with Interfaces.C; use Interfaces.C;

package Gsk.Component_Transfer is

   type Gsk_Component_Transfer is new Glib.C_Boxed with null record;
   Null_Gsk_Component_Transfer : constant Gsk_Component_Transfer;

   function From_Object (Object : System.Address) return Gsk_Component_Transfer;
   function From_Object_Free (B : access Gsk_Component_Transfer'Class) return Gsk_Component_Transfer;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_Discrete
      (Self   : out Gsk_Component_Transfer;
       N      : Guint;
       Values : Gfloat_Array);
   --  Creates a new component transfer that applies a step function.
   --  The new value is computed as
   --  C' = values[k]
   --  where k is the smallest value such that
   --  k / n <= C < (k + 1) / n
   --  <figure> <picture> <source srcset="discrete-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  discrete" src="discrete-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of values
   --  @param Values Values

   function Gsk_Component_Transfer_New_Discrete
      (N      : Guint;
       Values : Gfloat_Array) return Gsk_Component_Transfer;
   --  Creates a new component transfer that applies a step function.
   --  The new value is computed as
   --  C' = values[k]
   --  where k is the smallest value such that
   --  k / n <= C < (k + 1) / n
   --  <figure> <picture> <source srcset="discrete-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  discrete" src="discrete-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of values
   --  @param Values Values

   procedure Gtk_New_Gamma
      (Self : out Gsk_Component_Transfer;
       Amp  : Interfaces.C.C_float;
       Exp  : Interfaces.C.C_float;
       Ofs  : Interfaces.C.C_float);
   --  Creates a new component transfer that applies a gamma transform.
   --  The new value is computed as
   --  C' = amp * pow (C, exp) + ofs
   --  <figure> <picture> <source srcset="gamma-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  gamma" src="gamma-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param Amp Amplitude
   --  @param Exp Exponent
   --  @param Ofs Offset

   function Gsk_Component_Transfer_New_Gamma
      (Amp : Interfaces.C.C_float;
       Exp : Interfaces.C.C_float;
       Ofs : Interfaces.C.C_float) return Gsk_Component_Transfer;
   --  Creates a new component transfer that applies a gamma transform.
   --  The new value is computed as
   --  C' = amp * pow (C, exp) + ofs
   --  <figure> <picture> <source srcset="gamma-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  gamma" src="gamma-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param Amp Amplitude
   --  @param Exp Exponent
   --  @param Ofs Offset

   procedure Gtk_New_Identity (Self : out Gsk_Component_Transfer);
   --  Creates a new component transfer that doesn't change the component
   --  value.
   --  <figure> <picture> <source srcset="identity-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  identity" src="identity-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20

   function Gsk_Component_Transfer_New_Identity return Gsk_Component_Transfer;
   --  Creates a new component transfer that doesn't change the component
   --  value.
   --  <figure> <picture> <source srcset="identity-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  identity" src="identity-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20

   procedure Gtk_New_Levels
      (Self : out Gsk_Component_Transfer;
       N    : Interfaces.C.C_float);
   --  Creates a new component transfer that limits the values of the
   --  component to `n` levels.
   --  The new value is computed as
   --  C' = (floor (C * n) + 0.5) / n
   --  <figure> <picture> <source srcset="levels-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  levels" src="levels-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of levels

   function Gsk_Component_Transfer_New_Levels
      (N : Interfaces.C.C_float) return Gsk_Component_Transfer;
   --  Creates a new component transfer that limits the values of the
   --  component to `n` levels.
   --  The new value is computed as
   --  C' = (floor (C * n) + 0.5) / n
   --  <figure> <picture> <source srcset="levels-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  levels" src="levels-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of levels

   procedure Gtk_New_Linear
      (Self : out Gsk_Component_Transfer;
       M    : Interfaces.C.C_float;
       B    : Interfaces.C.C_float);
   --  Creates a new component transfer that applies a linear transform.
   --  The new value is computed as
   --  C' = C * m + b
   --  <figure> <picture> <source srcset="linear-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  linear" src="linear-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param M Slope
   --  @param B Offset

   function Gsk_Component_Transfer_New_Linear
      (M : Interfaces.C.C_float;
       B : Interfaces.C.C_float) return Gsk_Component_Transfer;
   --  Creates a new component transfer that applies a linear transform.
   --  The new value is computed as
   --  C' = C * m + b
   --  <figure> <picture> <source srcset="linear-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  linear" src="linear-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param M Slope
   --  @param B Offset

   procedure Gtk_New_Table
      (Self   : out Gsk_Component_Transfer;
       N      : Guint;
       Values : Gfloat_Array);
   --  Creates a new component transfer that applies a piecewise linear
   --  function.
   --  The new value is computed as
   --  C' = values[k] + (C - k / (n - 1)) * n * (values[k + 1] - values[k])
   --  where k is the smallest value such that
   --  k / (n - 1) <= C < (k + 1) / (n - 1)
   --  <figure> <picture> <source srcset="table-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  table" src="table-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of values
   --  @param Values Values

   function Gsk_Component_Transfer_New_Table
      (N      : Guint;
       Values : Gfloat_Array) return Gsk_Component_Transfer;
   --  Creates a new component transfer that applies a piecewise linear
   --  function.
   --  The new value is computed as
   --  C' = values[k] + (C - k / (n - 1)) * n * (values[k + 1] - values[k])
   --  where k is the smallest value such that
   --  k / (n - 1) <= C < (k + 1) / (n - 1)
   --  <figure> <picture> <source srcset="table-dark.png"
   --  media="(prefers-color-scheme: dark)"> <img alt="Component transfer:
   --  table" src="table-light.png"> </picture> </figure>
   --  Since: gtk+ 4.20
   --  @param N Number of values
   --  @param Values Values

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_component_transfer_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : Gsk_Component_Transfer) return Gsk_Component_Transfer;
   --  Creates a copy of Other.
   --  Since: gtk+ 4.20
   --  @return a newly allocated copy of Other. Has transfer-ownership='full'.

   procedure Free (Self : Gsk_Component_Transfer);
   --  Frees a component transfer.
   --  Since: gtk+ 4.20

   ---------------
   -- Functions --
   ---------------

   function Equal
      (Self  : System.Address;
       Other : System.Address) return Boolean;
   --  Compares two component transfers for equality.
   --  Since: gtk+ 4.20
   --  @param Self a component transfer
   --  @param Other another component transfer
   --  @return true if Self and Other are equal

private
   Null_Gsk_Component_Transfer : constant Gsk_Component_Transfer :=
      (Glib.C_Boxed with null record);

end Gsk.Component_Transfer;

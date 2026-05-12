
with Interfaces;

package OpenGL is

   type GLint is new Interfaces.Integer_32;
   subtype GLsizei is GLint range 0 .. GLint'Last;

   type GLfloat is new Interfaces.IEEE_Float_32;

   type GLfloat_Vector_3 is array (Positive range 1 .. 3) of aliased GLfloat;

   type GLfloat_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of aliased GLfloat
       with Convention => Fortran;

   type GLbitfield is new Interfaces.Unsigned_32;

   type GLenum is new Interfaces.Unsigned_32;

end OpenGL;

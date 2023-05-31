// imports
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <stdio.h> // for printf (for debugging purposes-i keep getting segfaulted)
#define WIDTH 400 // may change this

// global vars, copied over from image_blur_driver.c
int xwidth = WIDTH, ywidth = WIDTH;
int xw[1], yw[1]; // size of the image????
int temp[160000];

// the fortran subroutine, may change this function header
extern void diffuse_(int *, int*, int *);


// OpenGL helper functions
void display(void) {
  float rp[2];

  diffuse_(xw, yw, temp);

  glClear(GL_COLOR_BUFFER_BIT);

  rp[0] = -((float)*xw)/xwidth;
  rp[1] = -((float)*yw)/ywidth;
  glRasterPos2fv(rp);
  glDrawPixels(*xw, *yw, GL_RGBA, GL_UNSIGNED_BYTE, temp);
  

  glFlush();
  glutSwapBuffers();
}

void reshape(int w, int h) {
  xwidth = w;
  ywidth = h;
  glViewport(0,0,w,h);
}

int main(int argc, char *argv[]) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(WIDTH, WIDTH);
  glutInitWindowPosition(50,50);
  glutCreateWindow("Heat Diffusion"); // maybe change this
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glDisable(GL_LIGHTING);
  glutReshapeFunc(reshape);
  glutIdleFunc(display);
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}


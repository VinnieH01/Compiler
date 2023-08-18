#include "include/raylib.h"

void BindingClearBackground(Color* col)
{
	ClearBackground(*col);
}

void BindingDrawCircleV(Vector2* pos, float r, Color* col) 
{
	DrawCircleV(*pos, r, *col);
}

void BindingDrawText(const char* text, int x, int y, int s, Color* col) 
{
	DrawText(text, x, y, s, *col);
}
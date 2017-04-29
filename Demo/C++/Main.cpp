//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
	: TForm(Owner)
{
	HighlighterPath = ExpandFileName("..\\..\\..\\..\\Highlighters\\");
	ColorPath = ExpandFileName("..\\..\\..\\..\\Colors\\");

	AddFileNamesFromPathIntoListBox(HighlighterPath, ListBoxHighlighters);
	AddFileNamesFromPathIntoListBox(ColorPath, ListBoxColors);

	if (ListBoxHighlighters->Items->IndexOf("C++.json") >= 0)
	{
		ListBoxHighlighters->Selected[ListBoxHighlighters->Items->IndexOf("C++.json")] = true;
	}

	if (ListBoxColors->Items->IndexOf("Default.json") >= 0)
	{
		ListBoxColors->Selected[ListBoxColors->Items->IndexOf("Default.json")] = true;
	}

	SetSelectedHighlighter();
	SetSelectedColor();
}

//---------------------------------------------------------------------------
void TMainForm::AddFileNamesFromPathIntoListBox(const String APath, TListBox *AListBox)
{
	TSearchRec LSearchRec;
	if (FindFirst(APath + "*.json", faNormal, LSearchRec) == NULL)
	{
		try
		{
			do
			{
				AListBox->AddItem(LSearchRec.Name, NULL);
			}
			while (FindNext(LSearchRec) == NULL);
		}
		__finally
		{
			FindClose(LSearchRec);
		}
	}
}

//---------------------------------------------------------------------------

void TMainForm::SetSelectedColor()
{
	if (ListBoxColors->ItemIndex >= 0)
	{
		Editor->Highlighter->Colors->LoadFromFile(ColorPath + ListBoxColors->Items->Strings[ListBoxColors->ItemIndex]);
	}
}

//---------------------------------------------------------------------------

void TMainForm::SetSelectedHighlighter()
{
	if (ListBoxHighlighters->ItemIndex >= 0)
	{
		Editor->Highlighter->LoadFromFile(HighlighterPath + ListBoxHighlighters->Items->Strings[ListBoxHighlighters->ItemIndex]);
	}
	Editor->Lines->Text = Editor->Highlighter->Sample;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ListBoxColorsClick(TObject *Sender)
{
	SetSelectedColor();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::ListBoxHighlightersClick(TObject *Sender)
{
	SetSelectedHighlighter();
}
//---------------------------------------------------------------------------


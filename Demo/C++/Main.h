//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

#include "BCEditor.Editor.hpp"
#include "BCEditor.Highlighter.hpp"
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
	TBCEditor *Editor;
    TListBox *ListBoxColors;
	TListBox *ListBoxHighlighters;
	TPanel *PanelLeft;
	TSplitter *SplitterVertical;
	TSplitter *SplitterHorizontal;
	void __fastcall ListBoxColorsClick(TObject *Sender);
	void __fastcall ListBoxHighlightersClick(TObject *Sender);
private:	// User declarations
	TFileName ColorPath, HighlighterPath;

	void AddFileNamesFromPathIntoListBox(const String APath, TListBox *AListBox);
	void SetSelectedColor();
	void SetSelectedHighlighter();

public:		// User declarations
	__fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif

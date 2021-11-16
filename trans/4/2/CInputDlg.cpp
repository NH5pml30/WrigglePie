// CInputDlg.cpp : implementation file
//
#include <pch.h>

#include "CInputDlg.h"

// CInputDlg dialog

IMPLEMENT_DYNAMIC(CInputDlg, CDialog)

CInputDlg::CInputDlg(CWnd *pParent /*=nullptr*/) : CDialog(IDD_DIALOG1, pParent), text(_T("")) {}

CInputDlg::~CInputDlg() {}

void CInputDlg::DoDataExchange(CDataExchange *pDX)
{
  CDialog::DoDataExchange(pDX);
  DDX_Text(pDX, IDC_EDIT1, text);
}

BEGIN_MESSAGE_MAP(CInputDlg, CDialog)
END_MESSAGE_MAP()

// CInputDlg message handlers

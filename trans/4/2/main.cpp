#include <pch.h>

#include <optional>
#include <format>
#include <sstream>

#include "CInputDlg.h"
#include "generated/parser.h"
#include "Tree.h"

class CMyFrame : public CFrameWnd
{
public:
  CImage img;

  CMyFrame()
  {
    Create(NULL, _T("Parser"));
  }

  void set_image(CImage &&new_img)
  {
    img.Attach(new_img.Detach());
    RECT rc = {0, 0, img.GetWidth(), img.GetHeight()};
    AdjustWindowRect(&rc, GetWindowLong(m_hWnd, GWL_STYLE), FALSE);
    SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top,
                  SWP_NOMOVE | SWP_NOZORDER);
  }

  DECLARE_MESSAGE_MAP()
  afx_msg void OnPaint()
  {
    CPaintDC dc(this);
    CRect rect;
    GetClientRect(&rect);
    img.Draw(dc.m_hDC, rect, Gdiplus::InterpolationModeLowQuality);
  }
};

class ParserApp : public CWinApp
{
  static constexpr IID IID_IDOT = {
      0xA1080582, 0xD33F, 0x486E, {0xBD, 0x5F, 0x58, 0x19, 0x89, 0xA3, 0xC7, 0xE9}};
  static constexpr CLSID CLSID_DOT = {
      0x55811839, 0x47B0, 0x4854, {0x81, 0xB5, 0x0A, 0x00, 0x31, 0xB8, 0xAC, 0xEC}};

  CMyFrame *wnd = nullptr;

  static void report_error(const std::string &msg, const std::string &title)
  {
    auto wmsg = std::wstring(msg.begin(), msg.end()),
         wtitle = std::wstring(title.begin(), title.end());
    MessageBox(NULL, wmsg.c_str(), wtitle.c_str(), 0);
  }

  static bool check_hresult(HRESULT hr, const char *msg)
  {
    if (FAILED(hr))
    {
      report_error(std::format("{}: {:#x}", msg, (DWORD)hr), msg);
      return true;
    }
    return false;
  }

  static std::string T_to_string(const TCHAR *text) {
    return std::string(CT2CA(text));
  }

  class CCommandLineInfoParser : public CCommandLineInfo
  {
  public:
    bool is_graphics = true;
    std::optional<std::string> expr;

    void ParseParam(const TCHAR *pszParam, BOOL bFlag, BOOL bLast) override {
      if (bFlag)
      {
        if (wcscmp(pszParam, L"g") == 0)
          is_graphics = true;
        else if (wcscmp(pszParam, L"c") == 0)
          is_graphics = false;
      }
      else if (!is_graphics && !expr.has_value())
      {
        expr = T_to_string(pszParam);
      }
    }
  };

  template<typename T, typename F>
  static auto make_unique_scope_guard(T *value, F &&deleter)
  {
    return std::unique_ptr<T, F>(value, std::forward<F>(deleter));
  }

  static void co_uninit(ParserApp *)
  {
    CoUninitialize();
  }

  std::optional<std::unique_ptr<ParserApp, decltype(co_uninit) &>> co_init_guard;

  static auto create_dot_instance() {
    IDOT *pIDOT;
    HRESULT hr = CoCreateInstance(CLSID_DOT, NULL, CLSCTX_ALL, IID_IDOT,
                          reinterpret_cast<LPVOID *>(&pIDOT));
    return check_hresult(hr, "CoCreateInstance failed")
               ? nullptr
               : make_unique_scope_guard(pIDOT, [](IDOT *arg) { arg->Release(); });
  }

  std::optional<std::pair<std::string, bool>> get_input() {
    CCommandLineInfoParser info{};
    ParseCommandLine(info);

    std::string input;

    if (info.is_graphics)
    {
      CInputDlg dialog;
      if (dialog.DoModal() != IDOK)
        return {};

      input = T_to_string(dialog.text);
    }
    else if (info.expr.has_value())
      input = *info.expr;
    else
    {
      std::cerr << "Usage: <exe> [{/g}|{/c <expression>}] (default = /g)";
      return {};
    }

    return std::make_pair(input, info.is_graphics);
  }

  std::optional<std::string> parse_and_dotify(const std::string &input, const std::string &filename) {
    auto ss = std::istringstream(input);
    std::unique_ptr<Tree> tree;
    try
    {
      tree = LALR_parser().parse(ss);
    }
    catch (std::exception &e)
    {
      std::string msg = print_exception(e);
      report_error(msg, "Parse failed!");
      return {};
    }

    auto dot = tree->to_dot();
    {
      std::ofstream out(filename);
      out << dot;
    }
    return dot;
  }

  bool pngify(auto &idot, const std::string &dot, const std::wstring &filename) {
    IBinaryImage *img;
    HRESULT hr = idot->ToPNG(A2BSTR(dot.c_str()), &img);
    if (check_hresult(hr, "Conversion to PNG failed"))
      return false;

    VARIANT_BOOL res;
    hr = img->Save(W2BSTR(filename.c_str()), &res);
    if (check_hresult(hr, "Could not save the image"))
      return false;
    return true;
  }

  bool show_image(const std::wstring &filename) {
    CImage img;
    HRESULT hr = img.Load(filename.c_str());
    if (check_hresult(hr, "Could not load the image back"))
      return false;

    wnd = new CMyFrame();
    m_pMainWnd = wnd;

    wnd->set_image(std::move(img));
    wnd->ShowWindow(SW_NORMAL);
    return true;
  }

public:
  BOOL InitInstance()
  {
    HRESULT hr;
    hr = CoInitialize(NULL);
    if (check_hresult(hr, "CoInitialize failed"))
      return FALSE;
    co_init_guard.emplace(make_unique_scope_guard(this, co_uninit));

    auto idot = create_dot_instance();
    if (!idot)
      return FALSE;

    auto inputg = get_input();
    if (!inputg.has_value())
      return FALSE;
    auto &&[input, is_graphics] = *inputg;

    auto dot = parse_and_dotify(input, "dot.dot");
    if (!dot.has_value())
      return FALSE;

    if (!pngify(idot, *dot, L"image.png"))
      return FALSE;

    if (!is_graphics)
      return FALSE;

    show_image(L"image.png");
    return TRUE;
  }
};

ParserApp theApp;
BEGIN_MESSAGE_MAP(CMyFrame, CFrameWnd)
ON_WM_PAINT()
END_MESSAGE_MAP()

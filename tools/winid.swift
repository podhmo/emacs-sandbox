// winid.swift -- print the CGWindowID of the largest on-screen layer-0 window of PID
// usage: swift winid.swift <pid>
// exit: 0 found / 1 no window / 2 usage / 3 no Screen Recording permission
//
// NS emacs exposes no CGWindowID via frame parameters (window-id is an
// internal counter), so look it up from outside.
import CoreGraphics
import Foundation

guard CommandLine.arguments.count == 2, let pid = Int32(CommandLine.arguments[1]) else {
  FileHandle.standardError.write("usage: winid.swift <pid>\n".data(using: .utf8)!)
  exit(2)
}
// without the permission screencapture silently returns wallpaper only
guard CGPreflightScreenCaptureAccess() else { exit(3) }

let list = CGWindowListCopyWindowInfo([.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]] ?? []
let best = list
  .filter { ($0[kCGWindowOwnerPID as String] as? Int32) == pid && ($0[kCGWindowLayer as String] as? Int) == 0 }
  .max { a, b in
    func area(_ w: [String: Any]) -> Double {
      let r = w[kCGWindowBounds as String] as? [String: Double] ?? [:]
      return (r["Width"] ?? 0) * (r["Height"] ?? 0)
    }
    return area(a) < area(b)
  }
guard let w = best, let id = w[kCGWindowNumber as String] else { exit(1) }
print(id)

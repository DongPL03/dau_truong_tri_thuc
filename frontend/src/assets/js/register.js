// Thay vì chạy code tự do, hãy bọc nó vào một hàm và export

export function setupPasswordToggle() {
  if (typeof document === 'undefined') {
    return;
  }
  document.querySelectorAll(".toggle-password").forEach(icon => {
    icon.addEventListener("click", () => {
      const inputId = icon.getAttribute("data-target");
      const input = document.getElementById(inputId);

      if (!input) {
        console.error("Input target not found for:", inputId);
        return;
      }

      const isHidden = input.getAttribute("type") === "password";
      input.setAttribute("type", isHidden ? "text" : "password");

      // Đổi icon và tooltip
      icon.classList.toggle("fa-eye");
      icon.classList.toggle("fa-eye-slash");
      icon.title = isHidden ? "Ẩn mật khẩu" : "Hiện mật khẩu";
    });
  });
}

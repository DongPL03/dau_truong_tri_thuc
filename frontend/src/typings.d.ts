// src/typings.d.ts

// Khai báo module cho phép import từ đường dẫn tương đối đến thư mục gốc của project (src)
declare module 'src/assets/js/register.js' {
  export function setupPasswordToggle(): void; // Khai báo hàm bạn đã export
}

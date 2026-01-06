import { CommonModule } from '@angular/common';
import { HttpErrorResponse } from '@angular/common/http';
import { Component, ViewChild } from '@angular/core';
import { FormsModule, NgForm } from '@angular/forms';
import Swal from 'sweetalert2';
import { RegisterDto } from '../../dtos/nguoi-dung/register-dto';
import { ResponseObject } from '../../responses/response-object';
import { Base } from '../base/base';

@Component({
  selector: 'app-dang-ky',
  imports: [CommonModule, FormsModule],
  templateUrl: './dang-ky.html',
  styleUrl: './dang-ky.scss',
  standalone: true,
})
// implements AfterViewInit
export class DangKy extends Base {
  @ViewChild('registerForm') registerForm!: NgForm;

  // Biến dữ liệu
  tenDangNhap = '';
  email = '';
  password = '';
  retypePassword = '';
  hoTen = '';
  showPassword = false;

  // constructor(private renderer: Renderer2,
  //             @Inject(DOCUMENT) private override document: Document,
  //              private auth: AuthService) {
  // }
  // ngAfterViewInit(): void {
  //   // 2. Gọi hàm JS ngay sau khi View đã render xong
  //   setupPasswordToggle();
  //   console.log('Hàm JS đã chạy qua Module Alias!');
  // }

  constructor() {
    super();
    this.tenDangNhap = '';
    this.email = '';
    this.password = '';
    this.retypePassword = '';
    this.hoTen = '';
    this.showPassword = false;
  }

  togglePassword() {
    this.showPassword = !this.showPassword;
  }

  toggleRetypePassword() {
    this.showPassword = !this.showPassword;
  }

  checkPasswordsMatch() {
    if (this.password !== this.retypePassword) {
      this.registerForm.form.controls['retypePassword']?.setErrors({ passwordMismatch: true });
    } else {
      this.registerForm.form.controls['retypePassword']?.setErrors(null);
    }
  }

  register() {
    const dto = new RegisterDto({
      ten_dang_nhap: this.tenDangNhap,
      email: this.email,
      ho_ten: this.hoTen,
      password: this.password,
      retype_password: this.retypePassword,
      role_id: 1, // mặc định role USER
    });

    this.userService.register(dto).subscribe({
      next: (responseObject: ResponseObject) => {
        Swal.fire({
          title: 'Đăng ký thành công!',
          text: 'Mời bạn đăng nhập để tiếp tục.',
          icon: 'success',
          confirmButtonText: 'Đăng nhập',
        }).then(() => this.router.navigate(['']));
      },
      error: (err: HttpErrorResponse) => {
        // const msg = err?.error?.message || 'Đăng ký thất bại, vui lòng thử lại.';
        // Swal.fire('Lỗi đăng ký', msg, 'error').then(r => {
        // });
        console.error('Registration error:', err.error);
      },
    });
  }

  login() {
    this.router.navigate(['/login']).then((r) => {});
  }
}

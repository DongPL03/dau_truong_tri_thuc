import {Component, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import {Base} from '../base/base';
import {ToastrService} from 'ngx-toastr';
import Swal from 'sweetalert2';
import 'sweetalert2/src/sweetalert2.scss'
import {ResponseObject} from '../../responses/response-object';
import {RegisterDto} from '../../dtos/nguoi-dung/register-dto';
import { HttpErrorResponse } from '@angular/common/http';


@Component({
  selector: 'app-register',
  imports: [CommonModule, FormsModule],
  templateUrl: './register.html',
  styleUrl: './register.scss',
  standalone: true
})
export class Register extends Base
  // implements AfterViewInit
{
  @ViewChild('registerForm') registerForm!: NgForm;

  // Biến dữ liệu
  tenDangNhap = '';
  email = '';
  password = '';
  retypePassword = '';
  hoTen = '';
  diaChi = '';
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

  constructor(private toastr: ToastrService) {
    super();
    this.tenDangNhap = '';
    this.email = '';
    this.password = '';
    this.retypePassword = '';
    this.hoTen = '';
    this.diaChi = '';
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
      this.registerForm.form.controls['retypePassword']?.setErrors({passwordMismatch: true});
    } else {
      this.registerForm.form.controls['retypePassword']?.setErrors(null);
    }
  }

  register() {
    console.log("Register form data:", {
      tenDangNhap: this.tenDangNhap,
      email: this.email,
      password: this.password,
      retypePassword: this.retypePassword,
      hoTen: this.hoTen,
      diaChi: this.diaChi
    });

    // Swal.fire({
    //   title: 'Error!',
    //   text: 'Do you want to continue',
    //   icon: 'error',
    //   confirmButtonText: 'Cool'
    // })
    // form.control.markAllAsTouched();
    // if (form.invalid) {
    //   Swal.fire('Lỗi', 'Vui lòng điền đầy đủ thông tin hợp lệ', 'error').then(r => {
    //   });
    //   return;
    // }


    //
    const dto = new RegisterDto({
      ten_dang_nhap: this.tenDangNhap,
      email: this.email,
      ho_ten: this.hoTen,
      dia_chi: this.diaChi,
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
          confirmButtonText: 'Đăng nhập'
        })
          .then(() => this.router.navigate(['']));
      },
      error: (err: HttpErrorResponse) => {
        // const msg = err?.error?.message || 'Đăng ký thất bại, vui lòng thử lại.';
        // Swal.fire('Lỗi đăng ký', msg, 'error').then(r => {
        // });
        console.error('Registration error:', err.error);
      },
    });
  }
}

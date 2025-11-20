import {Component, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import {UserResponse} from '../../responses/nguoidung/user-response';
import {Base} from '../base/base';
import {LoginDTO} from '../../dtos/nguoi-dung/login-dto';
import {catchError, finalize, of, switchMap, tap} from 'rxjs';
import {ResponseObject} from '../../responses/response-object';
import {HttpErrorResponse} from '@angular/common/http';
import Swal from 'sweetalert2';


@Component({
  selector: 'app-dang-nhap',
  imports: [CommonModule, FormsModule],
  templateUrl: './dang-nhap.html',
  styleUrl: './dang-nhap.scss',
  standalone: true
})
export class DangNhap extends Base {
  @ViewChild('loginForm') loginForm!: NgForm;

  usernameOrEmail = '';
  password = '';
  rememberMe = false;
  showPassword = false;
  loading = false;
  userResponse?: UserResponse

  createAccount() {
    // Chuyển hướng người dùng đến trang đăng ký (hoặc trang tạo tài khoản)
    this.router.navigate(['/register']).then(r => {
    });
  }

  togglePassword() {
    this.showPassword = !this.showPassword;
  }

  login() {
    this.userService.getIdVaiTro(this.usernameOrEmail).subscribe({
      next: (roleId: number) => {
        console.log('roleId nhận được:', roleId);
        const loginDTO = new LoginDTO({
          ...(this.usernameOrEmail.includes('@')
            ? {email: this.usernameOrEmail}
            : {ten_dang_nhap: this.usernameOrEmail}),
          password: this.password,
          role_id: roleId,
        });
        console.log('LoginDTO:', loginDTO);
        this.userService.login(loginDTO).pipe(
          tap((apiResponse: ResponseObject) => {
            const {token} = apiResponse.data;
            this.tokenService.setTokens(token);
          }),
          switchMap((apiResponse: ResponseObject) => {
            const {token} = apiResponse.data;
            return this.userService.getUserDetail(token).pipe(
              tap((apiResponse2: ResponseObject) => {
                this.userResponse = {
                  ...apiResponse2.data
                };

                // if (this.rememberMe) {
                this.userService.saveUserResponseToLocalStorage(this.userResponse);
                // }
                console.log('Thông tin người dùng:', this.userResponse);
                console.log('Vai trò người dùng:', this.userResponse?.vai_tro?.ten_vai_tro);
                if (this.userResponse?.vai_tro?.ten_vai_tro === 'admin') {
                  this.router.navigate(['/admin']).then(r => {
                  });
                } else if (this.userResponse?.vai_tro?.ten_vai_tro === 'user') {
                  console.log('Đăng nhập thành công với vai trò nguoidung');
                  this.router.navigate(['/']).then(r => {
                  });
                }
              }),
              catchError((error: HttpErrorResponse) => {
                console.error('Lỗi khi lấy thông tin người dùng:', error?.error?.message ?? '');
                return of(null); // Tiếp tục chuỗi Observable
              })
            );
          }),
          finalize(() => {
            this.loading = false;
          })
        ).subscribe({
          error: (error: HttpErrorResponse) => {
            Swal.fire({
              position: 'top-end',
              icon: "error",
              title: "Oops...",
              text: "Something went wrong!",
            }).then(r => {
            });
          }
        });
      },
    });
  }
}

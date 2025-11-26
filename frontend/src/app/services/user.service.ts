import {inject, Inject, Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {DOCUMENT} from '@angular/common';
import {Observable, of, switchMap, tap} from 'rxjs';
import {environment} from '../environments/environment';
import {TokenService} from './token.service';
import {RegisterDto} from '../dtos/nguoi-dung/register-dto';
import {ResponseObject} from '../responses/response-object';
import {LoginDTO} from '../dtos/nguoi-dung/login-dto';
import {UserResponse} from '../responses/nguoidung/user-response';
import {UpdateUserDTO} from '../dtos/nguoi-dung/update-user-dto';
import {UserSummaryResponse} from '../responses/nguoidung/user-summary-response';

@Injectable({providedIn: 'root'})
export class UserService {
  private readonly baseUrl = `${environment.apiBaseUrl}/users`;
  private apiUserDetail = `${environment.apiBaseUrl}/users/details`;

  private http = inject(HttpClient);
  localStorage?: Storage;

  constructor(@Inject(DOCUMENT) private document: Document, private tokenService: TokenService) {
    this.localStorage = document.defaultView?.localStorage;
  }

  // --- AUTH ---
  register(dto: RegisterDto): Observable<ResponseObject> {
    return this.http.post<ResponseObject>(`${this.baseUrl}/register`, dto);
  }

  login(dto: LoginDTO): Observable<ResponseObject<UserResponse>> {
    return this.http.post<ResponseObject>(`${this.baseUrl}/login`, dto).pipe(
      // 1. Lưu token trước
      tap((res) => {
        const token = res.data?.token;
        const refresh = res.data?.refresh_token;
        if (token) {
          this.tokenService.setTokens(token, refresh);
        }
      }),
      // 2. Sau khi có token, gọi ngay API lấy thông tin user (switchMap để chuyển luồng)
      switchMap((res) => {
        const token = res.data?.token;
        if (token) {
          // Gọi hàm getUserDetail có sẵn để lấy thông tin
          return this.getUserDetail(token).pipe(
            tap((userRes) => {
              // 3. Lưu thông tin user vào LocalStorage
              if (userRes.data) {
                this.saveUserResponseToLocalStorage(userRes.data);
              }
            })
          );
        } else {
          // Trường hợp không có token (login lỗi), trả về luồng cũ
          return of(res as any);
        }
      })
    );
  }

  logout(): void {
    this.removeUserFromLocalStorage();
    this.tokenService.clear();
  }

  refreshToken(refreshToken: string): Observable<ResponseObject> {
    return this.http.post<ResponseObject>(`${this.baseUrl}/refreshToken`, {refreshToken});
  }

  // --- USER INFO ---
  getIdVaiTro(usernameOrEmail: string): Observable<number> {
    return this.http.get<number>(`${this.baseUrl}/idVaiTro/${usernameOrEmail}`);
  }

  getUserDetail(token: string): Observable<ResponseObject<UserResponse>> {
    return this.http.post<ResponseObject<UserResponse>>(`${this.baseUrl}/details`, null, {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
        Authorization: `Bearer ${token}`,
      }),
    });
  }

  currentUser(): UserResponse | null {
    return this.getUserResponseFromLocalStorage();
  }

  getUserId(): number {
    const userResponse = this.getUserResponseFromLocalStorage();
    return userResponse ? userResponse.id : 0;
  }

  updateUserDetail(token: string, updateUserDTO: UpdateUserDTO): Observable<ResponseObject> {
    let userResponse = this.getUserResponseFromLocalStorage();
    return this.http.put<ResponseObject>(`${this.apiUserDetail}/${userResponse?.id}`, updateUserDTO, {
      headers: new HttpHeaders({
        'Content-Type': 'application/json',
        Authorization: `Bearer ${token}`
      })
    })
  }

  updateMe(userId: number, updateUserDTO: UpdateUserDTO): Observable<ResponseObject> {
    const token = this.tokenService.getAccessToken();
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      Authorization: `Bearer ${token ?? ''}`,
    });
    return this.http.put<ResponseObject>(`${this.apiUserDetail}/${userId}`, updateUserDTO, {headers});
  }

  uploadProfileImage(file: File): Observable<ResponseObject> {

    const token = this.tokenService.getAccessToken();
    const headers = new HttpHeaders({
      Authorization: `Bearer ${token ?? ''}`,
    });
    const form = new FormData();
    form.append('file', file);
    return this.http.post<ResponseObject>(`${this.baseUrl}/upload-profile-image`, form, {headers});
  }

  /** Đổi mật khẩu (PUT /users/change-password) */
  changePassword(oldPassword: string, newPassword: string): Observable<ResponseObject> {
    const token = this.tokenService.getAccessToken();
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      Authorization: `Bearer ${token ?? ''}`,
    });
    const body = {oldPassword, newPassword};
    return this.http.put<ResponseObject>(`${this.baseUrl}/change-password`, body, {headers});
  }

  getUserDetails(): Observable<ResponseObject<UserResponse>> {
    const token = this.tokenService.getAccessToken();
    return this.http.post<ResponseObject<UserResponse>>(
      `${environment.apiBaseUrl}/users/details`,
      null,
      {
        headers: new HttpHeaders({
          'Content-Type': 'application/json',
          Authorization: `Bearer ${token}`,
        }),
      }
    );
  }

  /**
   * 🔹 Lấy thông tin tổng quan của 1 user bất kỳ (dùng cho bảng xếp hạng)
   */
  getUserSummary(user_id: number) {
    return this.http.get<ResponseObject<UserSummaryResponse>>(
      `${this.baseUrl}/${user_id}/summary`,
      {
        headers: new HttpHeaders({
          'Content-Type': 'application/json',
          Authorization: `Bearer ${this.tokenService.getAccessToken()}`,
        }),
      }
    );
  }


  getUsers(params: { page: number; limit: number; keyword: string }): Observable<ResponseObject> {
    return this.http.get<ResponseObject>(this.baseUrl, {params});
  }

  // --- LOCAL STORAGE ---
  saveUserResponseToLocalStorage(userResponse?: UserResponse) {
    if (!userResponse) return;
    try {
      this.localStorage?.setItem('user', JSON.stringify(userResponse));
    } catch (err) {
      console.error('Error saving nguoidung:', err);
    }
  }

  getUserResponseFromLocalStorage(): UserResponse | null {
    try {
      const json = this.localStorage?.getItem('user');
      return json ? JSON.parse(json) : null;
    } catch {
      return null;
    }
  }

  removeUserFromLocalStorage(): void {
    this.localStorage?.removeItem('user');
  }
}

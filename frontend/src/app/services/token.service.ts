import {Inject, Injectable} from '@angular/core';
import {DOCUMENT} from '@angular/common';
import {JwtHelperService} from '@auth0/angular-jwt';

@Injectable({providedIn: 'root'})
export class TokenService {
  private readonly ACCESS_KEY = 'access_token';
  private readonly REFRESH_KEY = 'refresh_token';
  private jwtHelper = new JwtHelperService();
  private localStorage?: Storage;

  constructor(@Inject(DOCUMENT) private document: Document) {
    this.localStorage = document.defaultView?.localStorage;
  }

  public isAuthenticated(): boolean {
    const isTokenExpired = this.isTokenExpired();
    const isUserIdValid = this.getUserId() > 0;
    return !isTokenExpired && isUserIdValid;
  }

  // 🔹 Lưu token
  setTokens(access?: string, refresh?: string): void {
    if (access) this.localStorage?.setItem(this.ACCESS_KEY, access);
    if (refresh) this.localStorage?.setItem(this.REFRESH_KEY, refresh);
  }

  // 🔹 Đọc token
  getAccessToken(): string | null {
    return this.localStorage?.getItem(this.ACCESS_KEY) ?? null;
  }

  getRefreshToken(): string | null {
    return this.localStorage?.getItem(this.REFRESH_KEY) ?? null;
  }

  // 🔹 Xóa token
  clear(): void {
    this.localStorage?.removeItem(this.ACCESS_KEY);
    this.localStorage?.removeItem(this.REFRESH_KEY);
  }

  // 🔹 Decode userId từ claim (BE claim: "userId")
  // getUserId(): number {
  //   const token = this.getAccessToken();
  //   if (!token) return 0;
  //   const obj = this.jwtHelper.decodeToken(token) || {};
  //   return 'userId' in obj ? parseInt(obj['userId']) : 0;
  // }
  getUserId(): number {
    const token = this.getAccessToken();
    if (!token) return 0;

    const decodedToken = this.jwtHelper.decodeToken(token);

    if (!decodedToken) return 0;

    // Kiểm tra các trường có thể chứa ID (Backend thường dùng 'sub', 'id', hoặc 'userId')
    // Hãy ưu tiên 'userId' nếu backend bạn custom, nếu không thì thử các key khác
    if ('userId' in decodedToken) return parseInt(decodedToken['userId']);
    if ('user_id' in decodedToken) return parseInt(decodedToken['user_id']);
    if ('id' in decodedToken) return parseInt(decodedToken['id']);
    if ('sub' in decodedToken) {
      // Nếu sub là số thì parse, nếu là email/string thì phải sửa logic backend
      const sub = parseInt(decodedToken['sub']);
      return isNaN(sub) ? 0 : sub;
    }

    return 0;
  }

  // 🔹 Kiểm tra token còn hạn không
  isTokenExpired(): boolean {
    const token = this.getAccessToken();
    if (!token) return true;
    return this.jwtHelper.isTokenExpired(token);
  }

  // 🔹 Trả toàn bộ payload (tuỳ chọn)
  getDecodedToken(): any | null {
    const token = this.getAccessToken();
    return token ? this.jwtHelper.decodeToken(token) : null;
  }

  // 🔹 Lấy danh sách roles (tuỳ chọn)
  getRoles(): string[] {
    const token = this.getAccessToken();
    if (!token) return [];
    const decoded = this.jwtHelper.decodeToken(token);
    return Array.isArray(decoded?.roles) ? decoded.roles : [];
  }

  // 🔹 Kiểm tra hợp lệ tổng thể (tuỳ chọn)
  hasValidToken(): boolean {
    const token = this.getAccessToken();
    return !!token && !this.jwtHelper.isTokenExpired(token);
  }
}

import { HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { TokenService } from './token.service';

@Injectable({
  providedIn: 'root',
})
export class HttpUtilService {
  constructor(private tokenService: TokenService) {}

  /** 🔹 Header mặc định (cho các request public như dang-nhap/dang-ky) */
  createHeaders(): HttpHeaders {
    return new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept-Language': 'vi',
    });
  }

  /** 🔹 Header có Authorization (cho các request yêu cầu dang-nhap) */
  createAuthHeaders(): HttpHeaders {
    const token = this.tokenService.getAccessToken();
    return new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept-Language': 'vi',
      Authorization: `Bearer ${token}`,
    });
  }

  /** 🔹 Header cho upload file (không set Content-Type, để browser tự set multipart/form-data) */
  createUploadHeaders(): HttpHeaders {
    const token = this.tokenService.getAccessToken();
    return new HttpHeaders({
      'Accept-Language': 'vi',
      Authorization: `Bearer ${token}`,
    });
  }
}

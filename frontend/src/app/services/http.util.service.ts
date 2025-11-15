import {Injectable} from '@angular/core';
import {HttpHeaders} from '@angular/common/http';
import {TokenService} from './token.service';

@Injectable({
  providedIn: 'root',
})
export class HttpUtilService {
  constructor(private tokenService: TokenService) {
  }

  /** 🔹 Header mặc định (cho các request public như login/register) */
  createHeaders(): HttpHeaders {
    return new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept-Language': 'vi',
    });
  }

  /** 🔹 Header có Authorization (cho các request yêu cầu login) */
  createAuthHeaders(): HttpHeaders {
    const token = this.tokenService.getAccessToken();
    return new HttpHeaders({
      'Content-Type': 'application/json',
      'Accept-Language': 'vi',
      'Authorization': `Bearer ${token}`
    });
  }
}

import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import { ResponseObject } from '../responses/response-object';
import { RecommendationResponse } from '../responses/recommendation/recommendation-response';
import { HttpUtilService } from './http.util.service';

@Injectable({ providedIn: 'root' })
export class RecommendationService {
  private readonly api = `${environment.apiBaseUrl}/recommendations`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  getMyRecommendations(): Observable<ResponseObject<RecommendationResponse>> {
    return this.http.get<ResponseObject<RecommendationResponse>>(`${this.api}/me`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}



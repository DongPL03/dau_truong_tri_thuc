import {Injectable} from '@angular/core';
import {environment} from '../environments/environment';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {ApiResponse} from '../responses/api.response';

@Injectable({
  providedIn: 'root'
})
export class ChudeService {

  private apiBaseUrl = environment.apiBaseUrl;

  constructor(private http: HttpClient) {
  }

  getChuDe(page: number, limit: number): Observable<ApiResponse> {
    return this.http.get<ApiResponse>(`${environment.apiBaseUrl}/chuDe`);
  }

  getChiTietChuDe(id: number): Observable<ApiResponse> {
    return this.http.get<ApiResponse>(`${this.apiBaseUrl}/chuDe/${id}`);
  }

  xoaChuDe(id: number): Observable<ApiResponse> {
    return this.http.delete<ApiResponse>(`${this.apiBaseUrl}/chuDe/${id}`);
  }

  // capNhatChuDe(id: number, updatedCategory: UpdateCategoryDTO): Observable<ApiResponse> {
  //   return this.http.put<ApiResponse>(`${this.apiBaseUrl}/categories/${id}`, updatedCategory);
  // }
  //
  // themChuDe(insertCategoryDTO: InsertCategoryDTO): Observable<ApiResponse> {
  //   // Add a new category
  //   return this.http.post<ApiResponse>(`${this.apiBaseUrl}/categories`, insertCategoryDTO);
  // }
}

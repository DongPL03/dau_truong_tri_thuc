import { HttpClient } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { environment } from '../environments/environment';
import { InventoryItem, KhoDoResponse } from '../models/kho-do.model';
import { ResponseObject } from '../responses/response-object';

@Injectable({
  providedIn: 'root',
})
export class KhoDoService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.apiBaseUrl}/inventory`;

  /**
   * Lấy danh sách vật phẩm trong kho
   */
  getInventory(): Observable<KhoDoResponse> {
    return this.http.get<ResponseObject<KhoDoResponse>>(this.apiUrl).pipe(map((res) => res.data!));
  }

  /**
   * Lấy chi tiết một vật phẩm
   */
  getItemDetail(vatPhamId: number): Observable<InventoryItem> {
    return this.http
      .get<ResponseObject<InventoryItem>>(`${this.apiUrl}/${vatPhamId}`)
      .pipe(map((res) => res.data!));
  }
}

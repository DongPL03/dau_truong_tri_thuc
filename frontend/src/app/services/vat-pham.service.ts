import { HttpClient } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import {
  LoaiVatPham,
  MuaVatPhamDTO,
  MuaVatPhamResponse,
  ShopResponse,
  SuDungVatPhamDTO,
  SuDungVatPhamResponse,
  VatPham,
  VatPhamInventory,
} from '../models/vat-pham.model';

@Injectable({
  providedIn: 'root',
})
export class VatPhamService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.apiBaseUrl}/vat-pham`;

  /**
   * Lấy inventory (kho vật phẩm) của user hiện tại
   */
  getInventory(): Observable<VatPhamInventory[]> {
    return this.http.get<VatPhamInventory[]>(`${this.apiUrl}/inventory`);
  }

  /**
   * Lấy tất cả vật phẩm đang active (cho shop/display)
   */
  getAllItems(): Observable<VatPham[]> {
    return this.http.get<VatPham[]>(`${this.apiUrl}/all`);
  }

  /**
   * Sử dụng vật phẩm trong trận đấu
   */
  useItem(dto: SuDungVatPhamDTO): Observable<SuDungVatPhamResponse> {
    return this.http.post<SuDungVatPhamResponse>(`${this.apiUrl}/use`, dto);
  }

  /**
   * Sử dụng vật phẩm theo ID
   */
  useItemById(
    tranDauId: number,
    vatPhamId: number,
    cauHoiIndex?: number
  ): Observable<SuDungVatPhamResponse> {
    return this.useItem({
      tran_dau_id: tranDauId,
      vat_pham_id: vatPhamId,
      cau_hoi_index: cauHoiIndex,
    });
  }

  /**
   * Sử dụng vật phẩm theo loại
   */
  useItemByType(
    tranDauId: number,
    loai: LoaiVatPham,
    cauHoiIndex?: number
  ): Observable<SuDungVatPhamResponse> {
    return this.useItem({
      tran_dau_id: tranDauId,
      loai_vat_pham: loai,
      cau_hoi_index: cauHoiIndex,
    });
  }

  /**
   * Lấy số lượng vật phẩm theo loại từ inventory
   */
  getItemQuantity(inventory: VatPhamInventory[], loai: LoaiVatPham): number {
    const item = inventory.find((i) => i.loai === loai);
    return item?.so_luong || 0;
  }

  /**
   * Kiểm tra có vật phẩm trong inventory không
   */
  hasItem(inventory: VatPhamInventory[], loai: LoaiVatPham): boolean {
    return this.getItemQuantity(inventory, loai) > 0;
  }

  // ==================== SHOP METHODS ====================

  /**
   * Lấy danh sách vật phẩm trong Shop
   */
  getShop(): Observable<ShopResponse> {
    return this.http.get<ShopResponse>(`${this.apiUrl}/shop`);
  }

  /**
   * Mua vật phẩm từ Shop
   */
  purchaseItem(dto: MuaVatPhamDTO): Observable<MuaVatPhamResponse> {
    return this.http.post<MuaVatPhamResponse>(`${this.apiUrl}/shop/purchase`, dto);
  }

  /**
   * Mua vật phẩm theo ID
   */
  purchaseItemById(vatPhamId: number, soLuong: number = 1): Observable<MuaVatPhamResponse> {
    return this.purchaseItem({
      vat_pham_id: vatPhamId,
      so_luong: soLuong,
    });
  }
}

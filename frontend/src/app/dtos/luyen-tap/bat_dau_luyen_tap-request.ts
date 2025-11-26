export class BatDauLuyenTapRequest {
  bo_cau_hoi_id?: number;
  so_luong?: number; // optional, default backend = 10

  constructor(data?: Partial<BatDauLuyenTapRequest>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}

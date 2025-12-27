import { BoCauHoiTrongKhoaResponse } from './bo-cau-hoi-trong-khoa-response';
import { KhoaHoiResponse } from './khoa-hoi-response';
import { TienDoKhoaHoiResponse } from './tien-do-khoa-hoi-response';

export interface KhoaHoiDetailResponse {
  khoa_hoc: KhoaHoiResponse;
  danh_sach_bo_cau_hoi: BoCauHoiTrongKhoaResponse[];
  tien_do?: TienDoKhoaHoiResponse;
  da_mo_khoa_khoa_hoc?: boolean;
}

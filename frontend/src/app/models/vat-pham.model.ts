/**
 * Enum các loại vật phẩm (Power-ups) trong trận đấu
 */
export enum LoaiVatPham {
  X2_DIEM = 'X2_DIEM',
  DONG_BANG_THOI_GIAN = 'DONG_BANG_THOI_GIAN',
  BO_QUA_CAU_HOI = 'BO_QUA_CAU_HOI',
  GOI_Y_50_50 = 'GOI_Y_50_50',
  KHIEN_BAO_VE = 'KHIEN_BAO_VE',
  X3_DIEM = 'X3_DIEM',
  HIEN_DAP_AN = 'HIEN_DAP_AN',
}

/**
 * Model vật phẩm (Power-up)
 */
export interface VatPham {
  id: number;
  ten: string;
  mo_ta: string;
  loai: LoaiVatPham;
  gia_tri_hieu_ung: number;
  thoi_gian_hieu_luc_giay: number;
  icon: string;
  mau_sac: string;
  gia_xu: number;
  kich_hoat: boolean;
  do_hiem: 'COMMON' | 'RARE' | 'EPIC' | 'LEGENDARY';
}

/**
 * Vật phẩm trong inventory của user
 */
export interface VatPhamInventory {
  vat_pham_id: number;
  ten: string;
  mo_ta: string;
  loai: LoaiVatPham;
  icon: string;
  mau_sac: string;
  do_hiem: string;
  so_luong: number;
  gia_tri_hieu_ung: number;
  thoi_gian_hieu_luc_giay: number;
}

/**
 * DTO để sử dụng vật phẩm
 */
export interface SuDungVatPhamDTO {
  tran_dau_id: number;
  vat_pham_id?: number;
  loai_vat_pham?: LoaiVatPham;
  cau_hoi_index?: number;
}

/**
 * Hiệu ứng khi sử dụng vật phẩm
 */
export interface HieuUngVatPham {
  he_so_diem?: number;
  thoi_gian_them_giay?: number;
  dap_an_bi_loai?: string[];
  bao_ve_combo?: boolean;
  dap_an_dung?: string;
  bo_qua_thanh_cong?: boolean;
}

/**
 * Response khi sử dụng vật phẩm
 */
export interface SuDungVatPhamResponse {
  thanh_cong: boolean;
  loai_vat_pham: LoaiVatPham;
  ten_vat_pham: string;
  thong_bao: string;
  hieu_ung?: HieuUngVatPham;
  so_luong_con_lai: number;
}

/**
 * Helper functions for power-ups
 */
export const VatPhamUtils = {
  /**
   * Lấy icon mặc định cho loại vật phẩm
   */
  getDefaultIcon(loai: LoaiVatPham): string {
    switch (loai) {
      case LoaiVatPham.X2_DIEM:
        return '⚡';
      case LoaiVatPham.X3_DIEM:
        return '💎';
      case LoaiVatPham.DONG_BANG_THOI_GIAN:
        return '❄️';
      case LoaiVatPham.GOI_Y_50_50:
        return '🎯';
      case LoaiVatPham.KHIEN_BAO_VE:
        return '🛡️';
      case LoaiVatPham.BO_QUA_CAU_HOI:
        return '⏭️';
      case LoaiVatPham.HIEN_DAP_AN:
        return '👁️';
      default:
        return '🎁';
    }
  },

  /**
   * Lấy màu sắc mặc định
   */
  getDefaultColor(loai: LoaiVatPham): string {
    switch (loai) {
      case LoaiVatPham.X2_DIEM:
        return '#FFD700';
      case LoaiVatPham.X3_DIEM:
        return '#E6E6FA';
      case LoaiVatPham.DONG_BANG_THOI_GIAN:
        return '#00BFFF';
      case LoaiVatPham.GOI_Y_50_50:
        return '#9932CC';
      case LoaiVatPham.KHIEN_BAO_VE:
        return '#228B22';
      case LoaiVatPham.BO_QUA_CAU_HOI:
        return '#FF6347';
      case LoaiVatPham.HIEN_DAP_AN:
        return '#FF1493';
      default:
        return '#808080';
    }
  },

  /**
   * Lấy màu theo độ hiếm
   */
  getRarityColor(doHiem: string): string {
    switch (doHiem) {
      case 'LEGENDARY':
        return '#FF8C00';
      case 'EPIC':
        return '#9400D3';
      case 'RARE':
        return '#4169E1';
      default:
        return '#808080';
    }
  },

  /**
   * Lấy tên hiển thị
   */
  getDisplayName(loai: LoaiVatPham): string {
    switch (loai) {
      case LoaiVatPham.X2_DIEM:
        return 'Nhân đôi điểm';
      case LoaiVatPham.X3_DIEM:
        return 'Nhân ba điểm';
      case LoaiVatPham.DONG_BANG_THOI_GIAN:
        return 'Đóng băng thời gian';
      case LoaiVatPham.GOI_Y_50_50:
        return 'Gợi ý 50/50';
      case LoaiVatPham.KHIEN_BAO_VE:
        return 'Khiên bảo vệ';
      case LoaiVatPham.BO_QUA_CAU_HOI:
        return 'Bỏ qua câu hỏi';
      case LoaiVatPham.HIEN_DAP_AN:
        return 'Tiết lộ đáp án';
      default:
        return 'Vật phẩm';
    }
  },
};

// ==================== SHOP INTERFACES ====================

/**
 * DTO để mua vật phẩm từ Shop
 */
export interface MuaVatPhamDTO {
  vat_pham_id: number;
  so_luong?: number;
}

/**
 * Vật phẩm trong Shop
 */
export interface ShopItem {
  id: number;
  ten: string;
  mo_ta: string;
  loai: string;
  icon: string;
  mau_sac: string;
  gia_xu: number;
  do_hiem: 'COMMON' | 'RARE' | 'EPIC' | 'LEGENDARY';
  co_the_mua: boolean;
  so_luong_con_lai_tuan: number;
  thong_bao_gioi_han?: string;
}

/**
 * Response từ Shop API
 */
export interface ShopResponse {
  vat_pham_list: ShopItem[];
  tien_vang_hien_tai: number;
}

/**
 * Response khi mua vật phẩm
 */
export interface MuaVatPhamResponse {
  thanh_cong: boolean;
  thong_bao: string;
  ten_vat_pham?: string;
  so_luong?: number;
  tong_gia?: number;
  tien_vang_con_lai?: number;
  so_luong_trong_inventory?: number;
}

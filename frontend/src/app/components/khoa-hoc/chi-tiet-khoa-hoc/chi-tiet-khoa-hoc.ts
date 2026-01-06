import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import Swal from 'sweetalert2';
import { UnlockBoCauHoiResponse } from '../../../responses/bocauhoi/unlock-bo-cau-hoi-response';
import { BoCauHoiTrongKhoaResponse } from '../../../responses/khoahoc/bo-cau-hoi-trong-khoa-response';
import { KhoaHoiDetailResponse } from '../../../responses/khoahoc/khoa-hoi-detail-response';
import { PhanTichHocTapResponse } from '../../../responses/khoahoc/phan-tich-hoc-tap-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';
import { DanhGiaComponent } from '../../shared/danh-gia/danh-gia';

@Component({
  selector: 'app-chi-tiet-khoa-hoc',
  standalone: true,
  imports: [CommonModule, DanhGiaComponent],
  templateUrl: './chi-tiet-khoa-hoc.html',
  styleUrl: './chi-tiet-khoa-hoc.scss',
})
export class ChiTietKhoaHoc extends Base implements OnInit {
  id!: number;
  detail?: KhoaHoiDetailResponse | null;
  loading = false;
  unlocking: { [key: number]: boolean } = {};
  unlockingKhoaHoc = false;
  phanTich?: PhanTichHocTapResponse | null;
  loadingPhanTich = false;

  ngOnInit(): void {
    this.id = Number(this.route.snapshot.paramMap.get('id'));
    this.loadDetail();
  }

  loadDetail() {
    this.loading = true;
    this.khoaHocService.getById(this.id).subscribe({
      next: (res: ResponseObject<KhoaHoiDetailResponse>) => {
        this.detail = res.data;
        // Debug: Log để kiểm tra da_mo_khoa
        console.log('Course detail loaded:', this.detail);
        if (this.detail?.danh_sach_bo_cau_hoi) {
          this.detail.danh_sach_bo_cau_hoi.forEach((bo, index) => {
            console.log(
              `Bo ${index + 1} (ID: ${bo.bo_cau_hoi_id}): da_mo_khoa = ${
                bo.da_mo_khoa
              }, trang_thai = ${bo.trang_thai}`
            );
          });
        }
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải chi tiết khóa học', 'error');
      },
    });
  }

  loadPhanTich() {
    this.loadingPhanTich = true;
    this.khoaHocService.getPhanTichKhoaHoc(this.id).subscribe({
      next: (res: ResponseObject<PhanTichHocTapResponse>) => {
        if (res.status === 404 || !res.data) {
          this.phanTich = null;
        } else {
          this.phanTich = res.data;
        }
        this.loadingPhanTich = false;
      },
      error: () => {
        this.loadingPhanTich = false;
        this.phanTich = null;
      },
    });
  }

  get boCauHoiList(): BoCauHoiTrongKhoaResponse[] {
    const list = this.detail?.danh_sach_bo_cau_hoi || [];
    // Sắp xếp theo thứ tự
    return [...list].sort((a, b) => a.thu_tu - b.thu_tu);
  }

  /**
   * Kiểm tra xem bộ câu hỏi có thể unlock không (logic tuần tự)
   */
  canUnlock(boCauHoi: BoCauHoiTrongKhoaResponse): boolean {
    if (boCauHoi.da_mo_khoa) {
      return false; // Đã unlock rồi
    }

    // Bộ câu hỏi đầu tiên (thu_tu nhỏ nhất) có thể unlock
    const sortedList = [...this.boCauHoiList].sort((a, b) => a.thu_tu - b.thu_tu);
    const firstBo = sortedList[0];
    if (boCauHoi.id === firstBo.id) {
      return true;
    }

    // Bộ câu hỏi tiếp theo chỉ unlock được nếu bộ trước đó đã unlock và hoàn thành
    const currentIndex = sortedList.findIndex((b) => b.id === boCauHoi.id);
    if (currentIndex > 0) {
      const previousBo = sortedList[currentIndex - 1];
      // Kiểm tra bộ trước đã unlock và hoàn thành
      return (
        previousBo.da_mo_khoa &&
        (previousBo.trang_thai === 'HOAN_THANH' || previousBo.trang_thai === 'DANG_HOC')
      );
    }

    return false;
  }

  /**
   * Unlock bộ câu hỏi trong khóa học
   */
  unlockBoCauHoi(boCauHoi: BoCauHoiTrongKhoaResponse) {
    if (!this.canUnlock(boCauHoi)) {
      Swal.fire('Thông báo', 'Bạn cần hoàn thành bộ câu hỏi trước đó trước', 'info');
      return;
    }

    if (boCauHoi.gia_mo_khoa && boCauHoi.gia_mo_khoa > 0) {
      Swal.fire({
        title: 'Xác nhận mở khóa?',
        html: `Bạn sẽ trả <strong>${boCauHoi.gia_mo_khoa}</strong> vàng để mở khóa bộ câu hỏi này.<br/>Bạn có chắc chắn?`,
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: 'Mở khóa',
        cancelButtonText: 'Hủy',
      }).then((result) => {
        if (result.isConfirmed) {
          this.doUnlock(boCauHoi);
        }
      });
    } else {
      this.doUnlock(boCauHoi);
    }
  }

  private doUnlock(boCauHoi: BoCauHoiTrongKhoaResponse) {
    this.unlocking[boCauHoi.bo_cau_hoi_id] = true;
    this.bocauHoiService.unlock_bo_cau_hoi(boCauHoi.bo_cau_hoi_id).subscribe({
      next: (res: ResponseObject<UnlockBoCauHoiResponse>) => {
        console.log('Unlock response:', res);
        this.unlocking[boCauHoi.bo_cau_hoi_id] = false;
        // Hiển thị thông báo trước
        Swal.fire({
          title: 'Thành công',
          text: 'Mở khóa bộ câu hỏi thành công!',
          icon: 'success',
          timer: 2000,
          showConfirmButton: false,
        });
        // Đợi một chút để đảm bảo transaction đã commit, sau đó reload
        // Tăng delay lên 1000ms để đảm bảo backend đã flush và commit
        setTimeout(() => {
          this.loadDetail();
        }, 1000);
      },
      error: (err) => {
        this.unlocking[boCauHoi.bo_cau_hoi_id] = false;
        const msg = err?.error?.message || 'Không thể mở khóa bộ câu hỏi';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /**
   * Navigate đến trang luyện tập với bộ câu hỏi trong khóa học
   */
  navigateToPractice(boCauHoiId: number) {
    const khoaHocId = this.route.snapshot.paramMap.get('id');
    if (khoaHocId) {
      this.router.navigate(['/khoa-hoc', khoaHocId, 'luyen-tap', boCauHoiId]);
    }
  }

  /**
   * Navigate đến trang chi tiết bộ câu hỏi
   */
  navigateToBoCauHoiDetail(boCauHoiId: number) {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', boCauHoiId]);
  }

  /**
   * Navigate đến trang luyện tập lại câu sai từ the_ghi_nho
   */
  navigateToPracticeFromMemo(boCauHoiId: number) {
    const khoaHocId = this.route.snapshot.paramMap.get('id');
    if (khoaHocId) {
      this.router.navigate(['/khoa-hoc', khoaHocId, 'luyen-tap', boCauHoiId, 'memo']);
    }
  }

  /**
   * Lấy class cho trạng thái bộ câu hỏi
   */
  getStatusClass(boCauHoi: BoCauHoiTrongKhoaResponse): string {
    if (boCauHoi.da_mo_khoa) {
      if (boCauHoi.trang_thai === 'HOAN_THANH') {
        return 'status-completed';
      } else if (boCauHoi.trang_thai === 'DANG_HOC') {
        return 'status-in-progress';
      }
      return 'status-unlocked';
    }
    return 'status-locked';
  }

  /**
   * Lấy icon cho trạng thái
   */
  getStatusIcon(boCauHoi: BoCauHoiTrongKhoaResponse): string {
    if (boCauHoi.da_mo_khoa) {
      if (boCauHoi.trang_thai === 'HOAN_THANH') {
        return 'fa-check-circle';
      } else if (boCauHoi.trang_thai === 'DANG_HOC') {
        return 'fa-play-circle';
      }
      return 'fa-unlock';
    }
    return 'fa-lock';
  }

  /**
   * Lấy text cho trạng thái
   */
  getStatusText(boCauHoi: BoCauHoiTrongKhoaResponse): string {
    if (boCauHoi.da_mo_khoa) {
      if (boCauHoi.trang_thai === 'HOAN_THANH') {
        return 'Đã hoàn thành';
      } else if (boCauHoi.trang_thai === 'DANG_HOC') {
        return 'Đang học';
      }
      return 'Đã mở khóa';
    }
    return 'Chưa mở khóa';
  }

  /**
   * Tính phần trăm hoàn thành khóa học
   */
  getCompletionPercentage(): number {
    if (!this.detail?.tien_do) {
      return 0;
    }
    return this.detail.tien_do.phan_tram_hoan_thanh || 0;
  }

  /**
   * Unlock khóa học bằng vàng
   */
  unlockKhoaHoc() {
    const giaMoKhoa = this.detail?.khoa_hoc?.gia_mo_khoa || 0;
    if (giaMoKhoa <= 0) {
      Swal.fire('Thông báo', 'Khóa học này không cần mở khóa', 'info');
      return;
    }

    Swal.fire({
      title: 'Xác nhận mở khóa khóa học?',
      html: `Bạn sẽ trả <strong>${giaMoKhoa}</strong> vàng để mở khóa khóa học này.<br/>Sau khi mở khóa, bạn sẽ có quyền truy cập vào tất cả các bộ câu hỏi trong khóa học (mở khóa tuần tự khi hoàn thành).<br/>Bạn có chắc chắn?`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Mở khóa',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.unlockingKhoaHoc = true;
        this.khoaHocService.unlockKhoaHoc(this.id).subscribe({
          next: (res: ResponseObject<any>) => {
            this.unlockingKhoaHoc = false;
            Swal.fire({
              title: 'Thành công',
              text: 'Mở khóa khóa học thành công!',
              icon: 'success',
              timer: 2000,
              showConfirmButton: false,
            });
            // Reload sau 1 giây để đảm bảo transaction đã commit
            setTimeout(() => {
              this.loadDetail();
              this.loadPhanTich();
            }, 1000);
          },
          error: (err) => {
            this.unlockingKhoaHoc = false;
            const msg = err?.error?.message || 'Không thể mở khóa khóa học';
            Swal.fire('Lỗi', msg, 'error');
          },
        });
      }
    });
  }

  /**
   * Gọi phân tích mạnh/yếu cho khóa học
   */
  runPhanTich() {
    this.loadingPhanTich = true;
    this.khoaHocService.phanTichKhoaHoc(this.id).subscribe({
      next: (res: ResponseObject<PhanTichHocTapResponse>) => {
        this.loadingPhanTich = false;
        this.phanTich = res.data ?? null;
        Swal.fire(
          'Thành công',
          'Đã phân tích khóa học dựa trên kết quả luyện tập của bạn.',
          'success'
        );
      },
      error: (err) => {
        this.loadingPhanTich = false;
        const msg = err?.error?.message || 'Không thể phân tích khóa học';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /**
   * Kiểm tra có phải chủ sở hữu khóa học không
   */
  isOwner(): boolean {
    const userId = this.tokenService.getUserId();
    return this.detail?.khoa_hoc?.nguoi_tao_id === userId;
  }

  /**
   * Kiểm tra có được phép đánh giá/comment không
   * - Chủ sở hữu không được đánh giá khóa học của mình
   * - Khóa học FREE: ai cũng được đánh giá
   * - Khóa học TRẢ PHÍ: chỉ người đã mở khóa mới được đánh giá
   */
  canRateAndComment(): boolean {
    if (!this.detail) return false;
    if (this.isOwner()) return false; // Chủ không tự đánh giá

    const gia = this.detail.khoa_hoc?.gia_mo_khoa ?? 0;
    // Nếu khóa học FREE (gia_mo_khoa = 0) → ai cũng rating được
    if (gia <= 0) return true;

    // Nếu khóa học TRẢ PHÍ → chỉ người đã mở khóa mới rating được
    return !!this.detail.da_mo_khoa_khoa_hoc;
  }
}

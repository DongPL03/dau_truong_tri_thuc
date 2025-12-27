import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { forkJoin } from 'rxjs';
import Swal from 'sweetalert2';
import { BoCauHoiResponse } from '../../../responses/bocauhoi/bocauhoi-response';
import { CauHoiResponse } from '../../../responses/cauhoi/cauhoi-response';
import { PageResponse } from '../../../responses/page-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';

@Component({
  selector: 'app-chi-tiet-bo-cau-hoi',
  imports: [CommonModule],
  templateUrl: './chi-tiet-bo-cau-hoi.html',
  styleUrl: './chi-tiet-bo-cau-hoi.scss',
  standalone: true,
})
export class BoCauHoiDetail extends Base implements OnInit {
  boCauHoiId!: number;
  bo?: BoCauHoiResponse | null;
  questions: CauHoiResponse[] = [];
  mediaUrl: string = 'assets/images/default-profile-image.jpeg';
  loading = true;
  isLocked = false;
  unlockError: string | null = null;
  unlocking = false;
  isPreview = false;
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/cauHoi/media/';
  currentUserId: number = 0;
  totalQuestions = 0;

  ngOnInit(): void {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('id'));
    this.currentUserId = this.tokenService.getUserId();
    this.fetchData();
  }

  fetchData() {
    this.loading = true;
    this.isLocked = false;
    this.unlockError = null;
    this.isPreview = false;

    // Gọi song song 2 API
    forkJoin({
      boCauHoi: this.bocauHoiService.getById(this.boCauHoiId),
      danhSachCau: this.cauHoiService.getByBoCauHoi(this.boCauHoiId),
    }).subscribe({
      next: (result) => {
        // 1. Xử lý thông tin bộ câu hỏi trước
        const resBo = result.boCauHoi as ResponseObject<BoCauHoiResponse>;
        this.bo = resBo.data;
        console.log('Bộ câu hỏi đã load:', this.bo);

        // 2. Xử lý danh sách câu hỏi sau khi đã có this.bo
        const resCau = result.danhSachCau as ResponseObject<PageResponse<CauHoiResponse>>;
        const allQuestions = resCau.data?.items ?? [];
        this.totalQuestions = allQuestions.length;

        console.log('Check quyền:', this.canViewAllQuestions()); // Lúc này this.bo đã có dữ liệu

        if (!this.canViewAllQuestions()) {
          this.questions = allQuestions.slice(0, 3);
          this.isPreview = allQuestions.length > this.questions.length;
        } else {
          this.questions = allQuestions;
        }
        this.loading = false;
      },
      error: (err) => {
        // Xử lý lỗi chung
        this.loading = false;
        const msg = err?.error?.message || '';
        if (msg.includes('mở khóa') || msg.includes('unlock')) {
          this.isLocked = true;
          this.unlockError = msg;
        }
        console.error(err);
      },
    });
  }

  /** Load thông tin cơ bản khi bộ bị lock (để hiển thị preview) */
  private loadBasicInfo() {
    // Có thể gọi API khác để lấy preview, hoặc dùng thông tin từ danh sách
    // Tạm thời để null, frontend sẽ hiển thị UI locked
  }

  /** Unlock bộ câu hỏi từ trang chi tiết */
  handleUnlock() {
    if (!this.bo) return;

    const price = this.bo.gia_mo_khoa ?? 0;
    Swal.fire({
      icon: 'question',
      title: 'Mở khoá bộ câu hỏi?',
      html: `
        <p>Bộ: <strong>${this.bo.tieu_de}</strong></p>
        <p>Giá mở khoá: <strong>${price} vàng</strong></p>
      `,
      showCancelButton: true,
      confirmButtonText: 'Mở khoá',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.doUnlock();
      }
    });
  }

  private doUnlock() {
    this.unlocking = true;
    this.bocauHoiService.unlock_bo_cau_hoi(this.boCauHoiId).subscribe({
      next: (res: ResponseObject<any>) => {
        this.unlocking = false;
        const data = res.data;
        Swal.fire({
          icon: 'success',
          title: data?.da_mo_khoa_truoc_do ? 'Đã mở khoá từ trước' : 'Mở khoá thành công!',
          html: `
            <p>Bộ: <strong>${this.bo?.tieu_de}</strong></p>
            <p>Đã trừ: <strong>${
              data?.da_mo_khoa_truoc_do ? 0 : data?.gia_mo_khoa
            } vàng</strong></p>
            <p>Vàng còn lại: <strong>${data?.tien_vang_sau}</strong></p>
          `,
          confirmButtonText: 'Xem chi tiết',
        }).then(() => {
          // Reload lại trang để hiển thị nội dung đã unlock
          this.fetchData();
        });
      },
      error: (err) => {
        this.unlocking = false;
        const msg = err?.error?.message || 'Không thể mở khoá bộ câu hỏi';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }

  /** Kiểm tra user có phải owner không */
  isOwner(): boolean {
    return this.bo?.nguoi_tao_id === this.currentUserId;
  }

  /** Kiểm tra user có phải admin không */
  isAdmin(): boolean {
    const roles = this.tokenService.getRoles();
    return roles.includes('ROLE_ADMIN');
  }

  /** Được xem toàn bộ danh sách câu hỏi? (owner, admin, hoặc đã unlock) */
  canViewAllQuestions(): boolean {
    if (!this.bo) return false;
    if (this.isOwner() || this.isAdmin()) return true;
    return !!this.bo.da_mo_khoa;
  }

  /** Được xem đáp án/giải thích? (chỉ owner hoặc admin) */
  canViewAnswers(): boolean {
    return this.isOwner() || this.isAdmin();
  }

  goCreateQuestion() {
    this.router
      .navigate(['/bo-cau-hoi', this.boCauHoiId, 'cau-hoi', 'tao-moi-cau-hoi'])
      .then((r) => {});
  }

  getMediaUrl(q: CauHoiResponse): string {
    if (!q.duong_dan_tep) return '';
    return `${this.imageBaseUrl}${q.duong_dan_tep}`;
  }

  navigateToEdit(id: number): void {
    this.router
      .navigateByUrl(`/bo-cau-hoi/${this.boCauHoiId}/cau-hoi/${id}/sua-bo-cau-hoi`)
      .then((r) => {});
  }

  onDeleteQuestion(id: number): void {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bạn có chắc muốn xóa câu hỏi này không?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.cauHoiService.delete(id).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Xóa câu hỏi thành công', 'success').then((r) => {});
            this.fetchData(); // reload lại danh sách
          },
          error: () => Swal.fire('Lỗi', 'Không thể xóa câu hỏi', 'error'),
        });
      }
    });
  }

  navigateToDetaiList() {
    this.router.navigateByUrl('/bo-cau-hoi/danh-sach-bo-cau-hoi').then((r) => {});
  }

  goToCourse(): void {
    if (!this.bo?.khoa_hoc_id) {
      return;
    }
    this.router.navigate(['/khoa-hoc', this.bo.khoa_hoc_id]).then((r) => {});
  }
}

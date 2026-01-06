import { CommonModule } from '@angular/common';
import { Component, EventEmitter, inject, Input, OnInit, Output } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import {
  CreateDanhGiaRequest,
  DanhGiaResponse,
  DanhGiaStatsResponse,
} from '../../../dtos/danhgia/danhgia.dto';
import { DanhGiaService } from '../../../services/danhgia.service';

@Component({
  selector: 'app-danh-gia',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './danh-gia.html',
  styleUrls: ['./danh-gia.scss'],
})
export class DanhGiaComponent implements OnInit {
  private danhGiaService = inject(DanhGiaService);

  @Input() loaiDoiTuong: 'BO_CAU_HOI' | 'KHOA_HOC' = 'BO_CAU_HOI';
  @Input() doiTuongId!: number;
  @Input() showReviews: boolean = true;
  @Input() allowRating: boolean = true;

  @Output() ratingSubmitted = new EventEmitter<DanhGiaResponse>();

  // Stats
  stats: DanhGiaStatsResponse | null = null;

  // My rating
  myRating: DanhGiaResponse | null = null;

  // Form
  selectedStar: number = 0;
  hoverStar: number = 0;
  reviewContent: string = '';
  isSubmitting: boolean = false;
  isEditing: boolean = false;

  // Reviews list
  reviews: DanhGiaResponse[] = [];
  currentPage: number = 0;
  totalPages: number = 0;
  isLoadingReviews: boolean = false;

  ngOnInit(): void {
    this.loadStats();
    this.loadMyRating();
    if (this.showReviews) {
      this.loadReviews();
    }
  }

  loadStats(): void {
    this.danhGiaService.getStats(this.loaiDoiTuong, this.doiTuongId).subscribe({
      next: (res) => {
        this.stats = res.data;
      },
      error: (err) => {
        console.error('Error loading stats:', err);
      },
    });
  }

  loadMyRating(): void {
    this.danhGiaService.getMyRating(this.loaiDoiTuong, this.doiTuongId).subscribe({
      next: (res) => {
        this.myRating = res.data;
        if (this.myRating) {
          this.selectedStar = this.myRating.so_sao;
          this.reviewContent = this.myRating.noi_dung || '';
        }
      },
      error: (err) => {
        console.error('Error loading my rating:', err);
      },
    });
  }

  loadReviews(page: number = 0): void {
    this.isLoadingReviews = true;
    this.danhGiaService.getRatings(this.loaiDoiTuong, this.doiTuongId, page, 5).subscribe({
      next: (res) => {
        const pageData = res.data;
        this.reviews = pageData.content;
        this.currentPage = pageData.number;
        this.totalPages = pageData.totalPages;
        this.isLoadingReviews = false;
      },
      error: (err) => {
        console.error('Error loading reviews:', err);
        this.isLoadingReviews = false;
      },
    });
  }

  onStarHover(star: number): void {
    if (this.allowRating && (!this.myRating || this.isEditing)) {
      this.hoverStar = star;
    }
  }

  onStarLeave(): void {
    this.hoverStar = 0;
  }

  onStarClick(star: number): void {
    if (!this.allowRating) return;
    if (this.myRating && !this.isEditing) {
      this.isEditing = true;
    }
    this.selectedStar = star;
  }

  submitRating(): void {
    if (this.selectedStar === 0) {
      Swal.fire('Lỗi', 'Vui lòng chọn số sao đánh giá', 'warning');
      return;
    }

    this.isSubmitting = true;
    const request: CreateDanhGiaRequest = {
      loai_doi_tuong: this.loaiDoiTuong,
      doi_tuong_id: this.doiTuongId,
      so_sao: this.selectedStar,
      noi_dung: this.reviewContent.trim() || undefined,
    };

    this.danhGiaService.createOrUpdate(request).subscribe({
      next: (res) => {
        this.isSubmitting = false;
        this.myRating = res.data;
        this.isEditing = false;
        this.ratingSubmitted.emit(res.data);
        this.loadStats();
        this.loadReviews();
        Swal.fire('Thành công', 'Đánh giá của bạn đã được ghi nhận', 'success');
      },
      error: (err) => {
        this.isSubmitting = false;
        Swal.fire('Lỗi', err.error?.message || 'Có lỗi xảy ra', 'error');
      },
    });
  }

  cancelEdit(): void {
    this.isEditing = false;
    if (this.myRating) {
      this.selectedStar = this.myRating.so_sao;
      this.reviewContent = this.myRating.noi_dung || '';
    }
  }

  deleteMyRating(): void {
    if (!this.myRating) return;

    Swal.fire({
      title: 'Xác nhận',
      text: 'Bạn có chắc muốn xóa đánh giá này?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#d33',
    }).then((result) => {
      if (result.isConfirmed && this.myRating) {
        this.danhGiaService.delete(this.myRating.id).subscribe({
          next: () => {
            this.myRating = null;
            this.selectedStar = 0;
            this.reviewContent = '';
            this.isEditing = false;
            this.loadStats();
            this.loadReviews();
            Swal.fire('Thành công', 'Đã xóa đánh giá', 'success');
          },
          error: (err) => {
            Swal.fire('Lỗi', err.error?.message || 'Có lỗi xảy ra', 'error');
          },
        });
      }
    });
  }

  getStarPercentage(star: number): number {
    if (!this.stats || this.stats.tong_danh_gia === 0) return 0;
    const count = this.stats.phan_bo_sao[star] || 0;
    return (count / this.stats.tong_danh_gia) * 100;
  }

  formatDate(dateStr: string): string {
    const date = new Date(dateStr);
    return date.toLocaleDateString('vi-VN', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
    });
  }

  prevPage(): void {
    if (this.currentPage > 0) {
      this.loadReviews(this.currentPage - 1);
    }
  }

  nextPage(): void {
    if (this.currentPage < this.totalPages - 1) {
      this.loadReviews(this.currentPage + 1);
    }
  }
}

import { CommonModule } from '@angular/common';
import { AfterViewInit, Component, ElementRef, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { ActivatedRoute, Router, RouterLink } from '@angular/router';
import Quill from 'quill';
import { Subject, takeUntil } from 'rxjs';
import { BaiVietDTO, LOAI_BAI_VIET_OPTIONS, LoaiBaiViet, Tag } from '../../../models/community';
import { CommunityService } from '../../../services/community.service';

@Component({
  selector: 'app-post-editor',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink],
  templateUrl: './post-editor.html',
  styleUrl: './post-editor.scss',
})
export class PostEditorComponent implements OnInit, AfterViewInit, OnDestroy {
  @ViewChild('editorContainer') editorContainer!: ElementRef;

  isEditMode = false;
  postId: number | null = null;

  // Form data
  tieuDe = '';
  loai: LoaiBaiViet = LoaiBaiViet.THAO_LUAN;
  selectedTags: Tag[] = [];

  // Quill editor
  quill: any = null;

  // Tags
  availableTags: Tag[] = [];
  showTagDropdown = false;
  tagSearch = '';

  // Images
  uploadedImages: { file: File; preview: string; uploaded?: boolean; url?: string }[] = [];
  isUploading = false;

  // Form state
  isSubmitting = false;
  isDirty = false;

  // Options
  loaiOptions = LOAI_BAI_VIET_OPTIONS;

  private destroy$ = new Subject<void>();

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private communityService: CommunityService
  ) {}

  ngOnInit(): void {
    this.loadTags();

    this.route.params.pipe(takeUntil(this.destroy$)).subscribe((params) => {
      if (params['id']) {
        this.isEditMode = true;
        this.postId = +params['id'];
        this.loadPost(this.postId);
      }
    });
  }

  ngAfterViewInit(): void {
    this.initQuillEditor();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();

    // Cleanup image previews
    this.uploadedImages.forEach((img) => {
      URL.revokeObjectURL(img.preview);
    });
  }

  initQuillEditor(): void {
    if (!Quill) {
      console.warn('Quill not loaded. Using textarea fallback.');
      return;
    }

    try {
      this.quill = new Quill(this.editorContainer.nativeElement, {
        theme: 'snow',
        placeholder: 'Viết nội dung bài viết...',
        modules: {
          toolbar: [
            [{ header: [1, 2, 3, false] }],
            ['bold', 'italic', 'underline', 'strike'],
            [{ color: [] }, { background: [] }],
            [{ list: 'ordered' }, { list: 'bullet' }],
            ['blockquote', 'code-block'],
            ['link', 'image'],
            ['clean'],
          ],
        },
      });

      this.quill.on('text-change', () => {
        this.isDirty = true;
      });
    } catch (error) {
      console.error('Failed to initialize Quill:', error);
    }
  }

  loadTags(): void {
    this.communityService
      .getAllTags()
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK') {
            this.availableTags = response.data || [];
          }
        },
      });
  }

  loadPost(postId: number): void {
    this.communityService
      .getPost(postId)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            const post = response.data;

            // Check ownership
            if (!post.laCuaToi) {
              this.router.navigate(['/community', postId]);
              return;
            }

            this.tieuDe = post.tieuDe;
            this.loai = post.loai as LoaiBaiViet;
            this.selectedTags = post.tags || [];

            // Set editor content
            if (this.quill) {
              this.quill.clipboard.dangerouslyPasteHTML(post.noiDung);
            }

            // Load existing images
            if (post.hinhAnh?.length) {
              post.hinhAnh.forEach((img) => {
                this.uploadedImages.push({
                  file: new File([], img.moTa || ''),
                  preview: img.duongDan,
                  uploaded: true,
                  url: img.duongDan,
                });
              });
            }

            this.isDirty = false;
          }
        },
        error: () => {
          this.router.navigate(['/community']);
        },
      });
  }

  // Tag management
  get filteredTags(): Tag[] {
    const search = this.tagSearch.toLowerCase();
    return this.availableTags.filter(
      (tag) =>
        !this.selectedTags.find((t) => t.id === tag.id) && tag.ten.toLowerCase().includes(search)
    );
  }

  toggleTagDropdown(): void {
    this.showTagDropdown = !this.showTagDropdown;
    if (!this.showTagDropdown) {
      this.tagSearch = '';
    }
  }

  addTag(tag: Tag): void {
    if (this.selectedTags.length >= 5) {
      console.log('Tối đa 5 tags cho mỗi bài viết');
      return;
    }
    this.selectedTags.push(tag);
    this.isDirty = true;
    this.showTagDropdown = false;
    this.tagSearch = '';
  }

  removeTag(tag: Tag): void {
    this.selectedTags = this.selectedTags.filter((t) => t.id !== tag.id);
    this.isDirty = true;
  }

  // Image management
  onImageSelect(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const files = Array.from(input.files);
    const maxFiles = 10 - this.uploadedImages.length;

    if (files.length > maxFiles) {
      console.log(`Chỉ có thể upload thêm ${maxFiles} hình ảnh`);
    }

    files.slice(0, maxFiles).forEach((file) => {
      if (!file.type.startsWith('image/')) {
        return;
      }

      if (file.size > 5 * 1024 * 1024) {
        console.log(`${file.name} vượt quá 5MB`);
        return;
      }

      const preview = URL.createObjectURL(file);
      this.uploadedImages.push({ file, preview });
      this.isDirty = true;
    });

    input.value = '';
  }

  removeImage(index: number): void {
    const img = this.uploadedImages[index];
    if (!img.uploaded) {
      URL.revokeObjectURL(img.preview);
    }
    this.uploadedImages.splice(index, 1);
    this.isDirty = true;
  }

  // Form validation
  get isValid(): boolean {
    const content = this.quill ? this.quill.root.innerHTML : '';
    return (
      this.tieuDe.trim().length >= 10 && content.trim().length > 20 && this.selectedTags.length > 0
    );
  }

  get validationErrors(): string[] {
    const errors: string[] = [];

    if (this.tieuDe.trim().length < 10) {
      errors.push('Tiêu đề phải có ít nhất 10 ký tự');
    }

    const content = this.quill ? this.quill.getText().trim() : '';
    if (content.length < 20) {
      errors.push('Nội dung phải có ít nhất 20 ký tự');
    }

    if (this.selectedTags.length === 0) {
      errors.push('Vui lòng chọn ít nhất 1 tag');
    }

    return errors;
  }

  // Submit
  async submit(): Promise<void> {
    if (!this.isValid || this.isSubmitting) return;

    this.isSubmitting = true;

    try {
      // Upload new images first
      const imagesToUpload = this.uploadedImages.filter((img) => !img.uploaded);

      if (imagesToUpload.length > 0) {
        this.isUploading = true;

        for (const img of imagesToUpload) {
          try {
            const response = await this.communityService.uploadImage(img.file).toPromise();
            if (response?.status === 'OK' && response.data) {
              img.uploaded = true;
              img.url = response.data;
            }
          } catch (error) {
            console.error('Failed to upload image:', error);
          }
        }

        this.isUploading = false;
      }

      // Prepare DTO
      const content = this.quill ? this.quill.root.innerHTML : '';

      const dto: BaiVietDTO = {
        tieuDe: this.tieuDe.trim(),
        noiDung: content,
        loai: this.loai,
        tagIds: this.selectedTags.map((t) => t.id),
        hinhAnhUrls: this.uploadedImages
          .filter((img) => img.uploaded && img.url)
          .map((img) => img.url!),
      };

      // Create or update post
      const request =
        this.isEditMode && this.postId
          ? this.communityService.updatePost(this.postId, dto)
          : this.communityService.createPost(dto);

      request.pipe(takeUntil(this.destroy$)).subscribe({
        next: (response) => {
          if (response.status === 'OK' && response.data) {
            this.isDirty = false;
            this.router.navigate(['/community', response.data.id]);
          } else {
            console.log(response.message || 'Có lỗi xảy ra');
          }
          this.isSubmitting = false;
        },
        error: (error) => {
          console.log(error.error?.message || 'Có lỗi xảy ra');
          this.isSubmitting = false;
        },
      });
    } catch (error) {
      console.error('Submit error:', error);
      this.isSubmitting = false;
    }
  }

  // Navigation guard
  canDeactivate(): boolean {
    if (!this.isDirty) return true;
    return confirm('Bạn có thay đổi chưa lưu. Bạn có chắc muốn rời đi?');
  }
}
